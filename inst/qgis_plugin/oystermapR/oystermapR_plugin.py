"""
oystermapR_plugin.py — Main QGIS plugin class.
Registers a toolbar button and menu item; manages the dialog and RRunner thread.
"""

import os

from qgis.PyQt.QtGui     import QIcon
from qgis.PyQt.QtWidgets import QAction
from qgis.core           import (
    QgsProject, QgsRasterLayer, QgsVectorLayer,
    QgsMessageLog, Qgis,
)

from .oystermapR_dialog import OystermapRDialog
from .r_runner          import RRunner


PLUGIN_DIR = os.path.dirname(__file__)


class OystermapRPlugin:
    """QGIS Plugin entry point."""

    def __init__(self, iface):
        self.iface   = iface
        self.canvas  = iface.mapCanvas()
        self.dlg     = None
        self.runner  = None
        self.actions = []

    # ──────────────────────────────────────────────────────────────────────────
    # QGIS lifecycle
    # ──────────────────────────────────────────────────────────────────────────
    def initGui(self):
        icon_path = os.path.join(PLUGIN_DIR, "icon.png")
        icon      = QIcon(icon_path) if os.path.exists(icon_path) else QIcon()

        action = QAction(icon, "oystermapR", self.iface.mainWindow())
        action.setToolTip("Predict oyster habitat suitability")
        action.triggered.connect(self._open_dialog)
        self.actions.append(action)

        self.iface.addToolBarIcon(action)
        self.iface.addPluginToRasterMenu("&oystermapR", action)

    def unload(self):
        for action in self.actions:
            self.iface.removePluginRasterMenu("&oystermapR", action)
            self.iface.removeToolBarIcon(action)
        self._stop_runner()

    # ──────────────────────────────────────────────────────────────────────────
    # Dialog
    # ──────────────────────────────────────────────────────────────────────────
    def _open_dialog(self):
        if self.dlg is None:
            self.dlg = OystermapRDialog(self.iface.mainWindow())
            self.dlg.run_requested.connect(self._start_run)
            self.dlg.stop_requested.connect(self._stop_runner)

        self.dlg.show()
        self.dlg.raise_()
        self.dlg.activateWindow()

    # ──────────────────────────────────────────────────────────────────────────
    # Runner management
    # ──────────────────────────────────────────────────────────────────────────
    def _start_run(self):
        if self.runner and self.runner.isRunning():
            return   # already running

        d = self.dlg
        self.runner = RRunner(
            rscript_exe = d.rscript_exe,
            csv_path    = d.csv_path,
            species     = d.species,
            out_dir     = d.out_dir,
        )
        self.runner.progress.connect(d.on_log_line)
        self.runner.error.connect(d.on_error)
        self.runner.finished.connect(self._on_runner_finished)
        self.runner.start()

    def _stop_runner(self):
        if self.runner:
            self.runner.abort()

    def _on_runner_finished(self, success, tif_path):
        if self.dlg:
            self.dlg.on_run_finished(success, tif_path)

        if success:
            self._load_outputs(tif_path)

    # ──────────────────────────────────────────────────────────────────────────
    # Layer loading
    # ──────────────────────────────────────────────────────────────────────────
    def _load_outputs(self, tif_path):
        d       = self.dlg
        species = d.species
        out_dir = d.out_dir
        safe    = species.replace(" ", "_")

        # ── GeoTIFF ─────────────────────────────────────────────────────────
        if d.load_tif and os.path.isfile(tif_path):
            layer_name = f"{species} suitability"
            rl = QgsRasterLayer(tif_path, layer_name)
            if rl.isValid():
                # Apply QML style if present
                qml_path = tif_path.replace(".tif", ".qml")
                if os.path.isfile(qml_path):
                    rl.loadNamedStyle(qml_path)
                    rl.triggerRepaint()
                QgsProject.instance().addMapLayer(rl)
                self.iface.setActiveLayer(rl)
                self.canvas.zoomToFullExtent()
                self._log(f"GeoTIFF layer added: {layer_name}")
            else:
                self._log(f"Could not load raster: {tif_path}", warning=True)

        # ── Contour GPKG ─────────────────────────────────────────────────────
        if d.load_gpkg:
            gpkg_path = os.path.join(out_dir, f"{safe}_contours.gpkg")
            if os.path.isfile(gpkg_path):
                vl = QgsVectorLayer(gpkg_path, f"{species} contours", "ogr")
                if vl.isValid():
                    QgsProject.instance().addMapLayer(vl)
                    self._log(f"Contour layer added: {species} contours")

        # ── PDF summary ───────────────────────────────────────────────────────
        pdf_path = os.path.join(out_dir, f"{safe}_summary.pdf")
        if os.path.isfile(pdf_path):
            import subprocess, sys
            try:
                if sys.platform == "darwin":
                    subprocess.Popen(["open", pdf_path])
                elif sys.platform.startswith("linux"):
                    subprocess.Popen(["xdg-open", pdf_path])
                else:
                    os.startfile(pdf_path)
            except Exception:
                pass   # silently skip if no viewer

    # ──────────────────────────────────────────────────────────────────────────
    # Logging
    # ──────────────────────────────────────────────────────────────────────────
    def _log(self, msg, warning=False):
        level = Qgis.Warning if warning else Qgis.Info
        QgsMessageLog.logMessage(msg, "oystermapR", level=level)
        if self.dlg:
            self.dlg.on_log_line(msg)
