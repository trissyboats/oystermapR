"""
r_runner.py — QThread worker that calls Rscript to run predict_oyster()
and streams stdout/stderr back to the main thread via Qt signals.
"""

import os
import subprocess
import tempfile

from qgis.PyQt.QtCore import QThread, pyqtSignal


# ---------------------------------------------------------------------------
# R script template — written to a temp file and executed by Rscript
# ---------------------------------------------------------------------------
_R_TEMPLATE = """
suppressPackageStartupMessages({{
  library(oystermapR)
}})

cat("oystermapR loaded.\\n")

df <- tryCatch(
  read.csv({csv_path!r}),
  error = function(e) stop("Cannot read CSV: ", conditionMessage(e))
)
cat("Data loaded: ", nrow(df), " rows\\n")

# ── Core prediction ──────────────────────────────────────────────────────────
tif_path <- file.path({out_dir!r}, {tif_name!r})
result <- predict_oyster(
  data           = df,
  species        = {species!r},
  output_geotiff = tif_path,
  verbose        = TRUE
)
cat("Prediction complete. Scored rows: ", sum(!result$excluded), "\\n")

# ── Contour lines ────────────────────────────────────────────────────────────
gpkg_path <- file.path({out_dir!r}, {gpkg_name!r})
tryCatch({{
  export_contours(result, output_gpkg = gpkg_path, verbose = FALSE)
  cat("Contours written: ", gpkg_path, "\\n")
}}, error = function(e) {{
  cat("Contour export skipped:", conditionMessage(e), "\\n")
}})

# ── Summary PDF ─────────────────────────────────────────────────────────────
pdf_path <- file.path({out_dir!r}, {pdf_name!r})
tryCatch({{
  generate_summary_pdf(
    result  = result,
    output  = pdf_path,
    species = {species!r},
    title   = {pdf_title!r},
    open    = FALSE,
    verbose = FALSE
  )
  cat("Summary PDF written: ", pdf_path, "\\n")
}}, error = function(e) {{
  cat("PDF summary skipped:", conditionMessage(e), "\\n")
}})

# ── Write result CSV so QGIS can read attributes ─────────────────────────────
csv_out <- file.path({out_dir!r}, {csv_out_name!r})
write.csv(result, csv_out, row.names = FALSE)
cat("Result CSV written: ", csv_out, "\\n")

cat("DONE\\n")
"""


class RRunner(QThread):
    """
    Background thread: runs Rscript with a generated prediction script.

    Signals
    -------
    progress(str)   — log lines streamed from Rscript stdout/stderr
    finished(bool, str)  — (success, tif_path)
    error(str)      — human-readable error message
    """

    progress = pyqtSignal(str)
    finished = pyqtSignal(bool, str)
    error    = pyqtSignal(str)

    def __init__(self, rscript_exe, csv_path, species, out_dir, parent=None):
        super().__init__(parent)
        self.rscript_exe = rscript_exe
        self.csv_path    = csv_path
        self.species     = species
        self.out_dir     = out_dir
        self._abort      = False

        safe_species = species.replace(" ", "_")
        self.tif_name     = f"{safe_species}_suitability.tif"
        self.gpkg_name    = f"{safe_species}_contours.gpkg"
        self.pdf_name     = f"{safe_species}_summary.pdf"
        self.csv_out_name = f"{safe_species}_result.csv"
        self.pdf_title    = f"Oyster Suitability — {species.replace('_', ' ').title()}"

    # ------------------------------------------------------------------
    def abort(self):
        self._abort = True
        if hasattr(self, "_proc") and self._proc:
            self._proc.terminate()

    # ------------------------------------------------------------------
    def run(self):
        # Write the R script to a temp file
        r_code = _R_TEMPLATE.format(
            csv_path     = self.csv_path,
            out_dir      = self.out_dir,
            tif_name     = self.tif_name,
            gpkg_name    = self.gpkg_name,
            pdf_name     = self.pdf_name,
            pdf_title    = self.pdf_title,
            csv_out_name = self.csv_out_name,
            species      = self.species,
        )

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".R", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(r_code)
            script_path = fh.name

        try:
            self._proc = subprocess.Popen(
                [self.rscript_exe, "--vanilla", script_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                encoding="utf-8",
                errors="replace",
                env=os.environ.copy(),
            )

            for line in self._proc.stdout:
                line = line.rstrip()
                if line:
                    self.progress.emit(line)
                if self._abort:
                    self._proc.terminate()
                    self.finished.emit(False, "")
                    return

            self._proc.wait()

            tif_path = os.path.join(self.out_dir, self.tif_name)
            if self._proc.returncode == 0 and os.path.exists(tif_path):
                self.finished.emit(True, tif_path)
            else:
                self.error.emit(
                    f"Rscript exited with code {self._proc.returncode}. "
                    "Check the log for details."
                )
                self.finished.emit(False, "")

        except FileNotFoundError:
            self.error.emit(
                f"Rscript not found at: {self.rscript_exe}\n"
                "Set the correct path in Plugin → Settings."
            )
            self.finished.emit(False, "")
        finally:
            try:
                os.unlink(script_path)
            except OSError:
                pass
