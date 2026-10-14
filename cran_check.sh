#!/bin/bash
# Clean CRAN check for oystermapR.
# Usage: bash cran_check.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PARENT="$(dirname "$SCRIPT_DIR")"
PKG_DIR="$SCRIPT_DIR"

PKG_VERSION=$(grep -m1 '^Version:' "$PKG_DIR/DESCRIPTION" | sed 's/Version:[[:space:]]*//')
TARBALL="oystermapR_${PKG_VERSION}.tar.gz"

echo "=== oystermapR CRAN check ==="
echo "Package : $PKG_DIR"
echo "Parent  : $PARENT"
echo "Version : $PKG_VERSION"
echo ""

# ── Step 1: Fix Rd files ──────────────────────────────────────────────────────
echo "--- Step 1: Fixing Rd files ---"
python3 "$PKG_DIR/fix_rd.py"
echo ""

# ── Step 2: Pre-build vignette HTML ──────────────────────────────────────────
echo "--- Step 2: Pre-rendering vignette ---"
Rscript --vanilla -e "
  pkg  <- '${PKG_DIR}'
  vign <- file.path(pkg, 'vignettes', 'example-bay-survey.Rmd')
  out  <- file.path(pkg, 'inst', 'doc')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(vign,
    output_format = rmarkdown::html_vignette(),
    output_dir    = out, quiet = TRUE)
  cat('Vignette OK\n')
" || echo "WARNING: vignette render failed — continuing"
echo ""

# ── Step 3: Remove old tarball ────────────────────────────────────────────────
echo "--- Step 3: Removing old tarball ---"
rm -fv "$PARENT"/oystermapR_*.tar.gz
echo ""

# ── Step 4: Build ─────────────────────────────────────────────────────────────
echo "--- Step 4: R CMD build ---"
cd "$PARENT"
R CMD build oystermapR
echo ""

# ── Step 5: Check ─────────────────────────────────────────────────────────────
echo "--- Step 5: R CMD check --as-cran ---"
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran "$TARBALL"

echo ""
echo "=== Done. Check log: ${PARENT}/oystermapR.Rcheck/00check.log ==="
