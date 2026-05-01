"""
oystermapR_dialog.py — Main plugin dialog (built entirely in code, no .ui file needed).
"""

import os

from qgis.PyQt.QtCore    import Qt, QSettings
from qgis.PyQt.QtGui     import QFont, QColor
from qgis.PyQt.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout,
    QLabel, QLineEdit, QPushButton, QComboBox,
    QFileDialog, QTextEdit, QGroupBox, QSizePolicy,
    QDialogButtonBox, QProgressBar, QTabWidget, QWidget,
    QCheckBox,
)


SPECIES_KEYS = [
    "ostrea_edulis",
    "magallana_gigas",
    "crassostrea_angulata",
    "ostrea_stentina",
    "ostrea_lurida",
]

SPECIES_DISPLAY = {
    "ostrea_edulis":      "Ostrea edulis — European Flat Oyster",
    "magallana_gigas":    "Magallana gigas — Pacific Oyster",
    "crassostrea_angulata": "Crassostrea angulata — Portuguese Oyster",
    "ostrea_stentina":    "Ostrea stentina — Denticulate Flat Oyster",
    "ostrea_lurida":      "Ostrea lurida — Olympia Oyster",
}

TEAL = "#1a6b6b"


class OystermapRDialog(QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("oystermapR — Oyster Habitat Suitability")
        self.setMinimumWidth(560)
        self.setMinimumHeight(560)
        self._build_ui()
        self._load_settings()

    # ──────────────────────────────────────────────────────────────────────────
    # UI construction
    # ──────────────────────────────────────────────────────────────────────────
    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setSpacing(8)

        # ── Header bar ────────────────────────────────────────────────────────
        header = QLabel(
            "<span style='color:white;font-size:15px;font-weight:700;'>"
            "🦪  oystermapR</span>"
            "<span style='color:rgba(255,255,255,0.7);font-size:10px;'>"
            "  v1.0.0  ·  Oyster Habitat Suitability</span>"
        )
        header.setStyleSheet(
            f"background:{TEAL};padding:10px 14px;border-radius:4px;"
        )
        root.addWidget(header)

        # ── Tabs ──────────────────────────────────────────────────────────────
        tabs = QTabWidget()
        root.addWidget(tabs, stretch=1)

        tabs.addTab(self._build_run_tab(),      "▶  Run")
        tabs.addTab(self._build_settings_tab(), "⚙  Settings")

        # ── Log ───────────────────────────────────────────────────────────────
        log_group = QGroupBox("Log")
        log_layout = QVBoxLayout(log_group)
        self.log_box = QTextEdit()
        self.log_box.setReadOnly(True)
        self.log_box.setFont(QFont("Courier", 9))
        self.log_box.setMinimumHeight(120)
        self.log_box.setStyleSheet("background:#1e1e1e;color:#d4d4d4;")
        log_layout.addWidget(self.log_box)
        root.addWidget(log_group)

        # ── Progress bar ──────────────────────────────────────────────────────
        self.progress_bar = QProgressBar()
        self.progress_bar.setRange(0, 0)   # indeterminate
        self.progress_bar.setVisible(False)
        root.addWidget(self.progress_bar)

        # ── Buttons ───────────────────────────────────────────────────────────
        btn_row = QHBoxLayout()
        self.run_btn  = QPushButton("▶  Run Analysis")
        self.run_btn.setStyleSheet(
            f"background:{TEAL};color:white;font-weight:bold;"
            "padding:8px 18px;border-radius:4px;"
        )
        self.stop_btn = QPushButton("■  Stop")
        self.stop_btn.setEnabled(False)
        self.close_btn = QPushButton("Close")
        btn_row.addWidget(self.run_btn)
        btn_row.addWidget(self.stop_btn)
        btn_row.addStretch()
        btn_row.addWidget(self.close_btn)
        root.addLayout(btn_row)

        # ── Wire buttons ──────────────────────────────────────────────────────
        self.run_btn.clicked.connect(self._on_run)
        self.stop_btn.clicked.connect(self._on_stop)
        self.close_btn.clicked.connect(self.reject)

    # ── Run tab ───────────────────────────────────────────────────────────────
    def _build_run_tab(self):
        w = QWidget()
        form = QFormLayout(w)
        form.setLabelAlignment(Qt.AlignRight)
        form.setSpacing(10)
        form.setContentsMargins(12, 12, 12, 12)

        # Survey CSV
        csv_row = QHBoxLayout()
        self.csv_edit = QLineEdit()
        self.csv_edit.setPlaceholderText("Path to survey CSV…")
        csv_browse = QPushButton("Browse…")
        csv_browse.clicked.connect(self._browse_csv)
        csv_row.addWidget(self.csv_edit)
        csv_row.addWidget(csv_browse)
        form.addRow("Survey CSV:", csv_row)

        # Output folder
        out_row = QHBoxLayout()
        self.out_edit = QLineEdit()
        self.out_edit.setPlaceholderText("Folder for GeoTIFF, contours, PDF…")
        out_browse = QPushButton("Browse…")
        out_browse.clicked.connect(self._browse_out)
        out_row.addWidget(self.out_edit)
        out_row.addWidget(out_browse)
        form.addRow("Output folder:", out_row)

        # Species
        self.species_combo = QComboBox()
        for key in SPECIES_KEYS:
            self.species_combo.addItem(SPECIES_DISPLAY[key], key)
        form.addRow("Species:", self.species_combo)

        # Report title
        self.title_edit = QLineEdit()
        self.title_edit.setPlaceholderText("e.g. Kames Bay Spring Survey 2025")
        form.addRow("Report title:", self.title_edit)

        # Checkboxes
        self.load_tif_chk  = QCheckBox("Load GeoTIFF into QGIS after run")
        self.load_tif_chk.setChecked(True)
        self.load_gpkg_chk = QCheckBox("Load contour lines into QGIS after run")
        self.load_gpkg_chk.setChecked(True)
        form.addRow("", self.load_tif_chk)
        form.addRow("", self.load_gpkg_chk)

        return w

    # ── Settings tab ──────────────────────────────────────────────────────────
    def _build_settings_tab(self):
        w = QWidget()
        form = QFormLayout(w)
        form.setLabelAlignment(Qt.AlignRight)
        form.setSpacing(10)
        form.setContentsMargins(12, 12, 12, 12)

        # Rscript path
        rs_row = QHBoxLayout()
        self.rscript_edit = QLineEdit()
        self.rscript_edit.setPlaceholderText(
            "e.g. /usr/bin/Rscript  or  C:/Program Files/R/R-4.x.x/bin/Rscript.exe"
        )
        rs_browse = QPushButton("Browse…")
        rs_browse.clicked.connect(self._browse_rscript)
        rs_row.addWidget(self.rscript_edit)
        rs_row.addWidget(rs_browse)
        form.addRow("Rscript path:", rs_row)

        note = QLabel(
            "<small>Tip: on macOS/Linux run <code>which Rscript</code> in a terminal.<br>"
            "On Windows: <code>C:\\Program Files\\R\\R-4.x.x\\bin\\Rscript.exe</code></small>"
        )
        note.setTextFormat(Qt.RichText)
        note.setWordWrap(True)
        form.addRow("", note)

        save_btn = QPushButton("Save settings")
        save_btn.clicked.connect(self._save_settings)
        form.addRow("", save_btn)

        return w

    # ──────────────────────────────────────────────────────────────────────────
    # Browse helpers
    # ──────────────────────────────────────────────────────────────────────────
    def _browse_csv(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Select survey CSV", "", "CSV files (*.csv);;All files (*)"
        )
        if path:
            self.csv_edit.setText(path)
            # Auto-fill output folder to same directory
            if not self.out_edit.text():
                self.out_edit.setText(os.path.dirname(path))

    def _browse_out(self):
        path = QFileDialog.getExistingDirectory(self, "Select output folder")
        if path:
            self.out_edit.setText(path)

    def _browse_rscript(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Locate Rscript executable", "",
            "Rscript (Rscript Rscript.exe);;All files (*)"
        )
        if path:
            self.rscript_edit.setText(path)

    # ──────────────────────────────────────────────────────────────────────────
    # Settings persistence
    # ──────────────────────────────────────────────────────────────────────────
    def _load_settings(self):
        s = QSettings("oystermapR", "oystermapR_plugin")
        self.rscript_edit.setText(s.value("rscript_path", "Rscript"))

    def _save_settings(self):
        s = QSettings("oystermapR", "oystermapR_plugin")
        s.setValue("rscript_path", self.rscript_edit.text().strip())
        self._log("Settings saved.")

    # ──────────────────────────────────────────────────────────────────────────
    # Log helper
    # ──────────────────────────────────────────────────────────────────────────
    def _log(self, text, colour=None):
        if colour:
            self.log_box.append(
                f"<span style='color:{colour};'>{text}</span>"
            )
        else:
            self.log_box.append(text)
        # Scroll to bottom
        sb = self.log_box.verticalScrollBar()
        sb.setValue(sb.maximum())

    # ──────────────────────────────────────────────────────────────────────────
    # Public properties used by the plugin
    # ──────────────────────────────────────────────────────────────────────────
    @property
    def csv_path(self):
        return self.csv_edit.text().strip()

    @property
    def out_dir(self):
        return self.out_edit.text().strip()

    @property
    def species(self):
        return self.species_combo.currentData()

    @property
    def report_title(self):
        t = self.title_edit.text().strip()
        return t if t else f"Oyster Suitability — {self.species.replace('_', ' ').title()}"

    @property
    def rscript_exe(self):
        return self.rscript_edit.text().strip() or "Rscript"

    @property
    def load_tif(self):
        return self.load_tif_chk.isChecked()

    @property
    def load_gpkg(self):
        return self.load_gpkg_chk.isChecked()

    # ──────────────────────────────────────────────────────────────────────────
    # Run / stop — these emit signals caught by the plugin class
    # ──────────────────────────────────────────────────────────────────────────
    def _on_run(self):
        # Validation
        if not self.csv_path:
            self._log("⚠  No CSV selected.", "#e67e22")
            return
        if not os.path.isfile(self.csv_path):
            self._log(f"⚠  CSV not found: {self.csv_path}", "#e67e22")
            return
        if not self.out_dir:
            self._log("⚠  No output folder selected.", "#e67e22")
            return
        os.makedirs(self.out_dir, exist_ok=True)

        self.run_btn.setEnabled(False)
        self.stop_btn.setEnabled(True)
        self.progress_bar.setVisible(True)
        self._log(f"Starting analysis: {self.species}", TEAL)
        self._log(f"CSV:    {self.csv_path}")
        self._log(f"Output: {self.out_dir}")
        # Signal caught by plugin — runs RRunner
        self.run_requested.emit()

    def _on_stop(self):
        self.stop_btn.setEnabled(False)
        self._log("⚠  Stop requested…", "#e67e22")
        self.stop_requested.emit()

    # Qt signals (defined at class level below)
    from qgis.PyQt.QtCore import pyqtSignal as _sig
    run_requested  = _sig()
    stop_requested = _sig()

    # Called by plugin when run finishes
    def on_run_finished(self, success, tif_path):
        self.run_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self.progress_bar.setVisible(False)
        if success:
            self._log("✓  Analysis complete.", "#27ae60")
            self._log(f"   GeoTIFF: {tif_path}", "#27ae60")
        else:
            self._log("✗  Analysis failed — see log above.", "#e74c3c")

    def on_log_line(self, line):
        self._log(line)

    def on_error(self, msg):
        self._log(f"✗  {msg}", "#e74c3c")
