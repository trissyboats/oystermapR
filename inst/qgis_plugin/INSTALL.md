# oystermapR QGIS Plugin — Installation

## Prerequisites

1. **QGIS 3.16 or later** — https://qgis.org/en/site/forusers/download.html
2. **R 4.1 or later** with oystermapR installed:
   ```r
   remotes::install_github("tristantucker48/oystermapR")
   ```

## Install the plugin

### Option A — Copy directly (recommended during development)

1. Find your QGIS plugins folder:
   - **macOS:** `~/Library/Application Support/QGIS/QGIS3/profiles/default/python/plugins/`
   - **Windows:** `%APPDATA%\QGIS\QGIS3\profiles\default\python\plugins\`
   - **Linux:** `~/.local/share/QGIS/QGIS3/profiles/default/python/plugins/`

2. Copy the `oystermapR` folder (this folder, containing `metadata.txt`) into that plugins directory.

3. In QGIS: **Plugins → Manage and Install Plugins → Installed** → tick **oystermapR** → OK.

### Option B — Install from ZIP

1. Zip the `oystermapR` folder: `zip -r oystermapR.zip oystermapR/`
2. In QGIS: **Plugins → Manage and Install Plugins → Install from ZIP** → select the zip → Install.

## First-time setup

1. Click the 🦪 toolbar button (or **Raster → oystermapR**).
2. Go to the **⚙ Settings** tab.
3. Set the **Rscript path**:
   - macOS/Linux: run `which Rscript` in Terminal — typically `/usr/local/bin/Rscript`
   - Windows: `C:\Program Files\R\R-4.x.x\bin\Rscript.exe`
4. Click **Save settings**.

## Running an analysis

1. **▶ Run tab** — Browse to your survey CSV.
2. Select an output folder (GeoTIFF, contours, and PDF will be written here).
3. Choose a species.
4. Click **▶ Run Analysis**.
5. Watch the log — when complete, the GeoTIFF and contour layers load automatically with the correct colour ramp.
6. The PDF summary opens in your default viewer.

## Outputs

| File | Description |
|------|-------------|
| `<species>_suitability.tif` | IDW-interpolated suitability raster (Band 1 = suitability 0–1) |
| `<species>_suitability.qml` | QGIS colour ramp style — drag onto the raster layer |
| `<species>_contours.gpkg` | Suitability contour lines (0.2, 0.45, 0.70) |
| `<species>_summary.pdf` | 4-page printable summary with heatmap |
| `<species>_result.csv` | Per-location scores and attributes |
