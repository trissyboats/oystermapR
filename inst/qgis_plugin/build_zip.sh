#!/usr/bin/env bash
# build_zip.sh — package the oystermapR QGIS plugin into an installable ZIP.
#
# Usage:
#   cd inst/qgis_plugin
#   bash build_zip.sh
#
# The output file oystermapR_qgis_plugin.zip is written to this directory.
# Install in QGIS via: Plugins -> Manage and Install Plugins -> Install from ZIP.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PLUGIN_SRC="$SCRIPT_DIR/oystermapR"
OUT_ZIP="$SCRIPT_DIR/oystermapR_qgis_plugin.zip"

if [ ! -d "$PLUGIN_SRC" ]; then
  echo "ERROR: plugin folder not found at $PLUGIN_SRC" >&2
  exit 1
fi

# Remove old ZIP if present
[ -f "$OUT_ZIP" ] && rm "$OUT_ZIP"

# Build ZIP — the top-level folder inside the archive must be named 'oystermapR'
cd "$SCRIPT_DIR"
zip -r "$OUT_ZIP" oystermapR \
  --exclude "oystermapR/__pycache__/*" \
  --exclude "oystermapR/*.pyc"

echo "Built: $OUT_ZIP  ($(du -h "$OUT_ZIP" | cut -f1))"
echo ""
echo "Install in QGIS:"
echo "  Plugins -> Manage and Install Plugins -> Install from ZIP -> select $OUT_ZIP"
