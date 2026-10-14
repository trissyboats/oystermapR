#!/usr/bin/env bash
# =============================================================================
# release_1.6.0.sh -- test, check and (optionally) submit oystermapR 1.6.0
#
#   bash release_1.6.0.sh            # steps 1-7: everything local, submits nothing
#   bash release_1.6.0.sh --winbuilder   # also upload to CRAN's win-builder (step 8)
#   bash release_1.6.0.sh --submit       # also submit to CRAN (step 9)
#
# Steps 1-7 change nothing outside this package folder and a scratch directory,
# and never touch the old tarballs in ../ (unlike cran_check.sh, which deletes
# ../oystermapR_*.tar.gz -- including the 1.5.0 one that is on CRAN).
#
# Step 8 uploads the tarball to win-builder and results come back by email to
# the maintainer. Step 9 submits to CRAN and cannot be undone; it asks you to
# type a phrase first. Neither runs unless you pass the flag.
# =============================================================================
set -euo pipefail

WINBUILDER=0; SUBMIT=0
for a in "$@"; do
  case "$a" in
    --winbuilder) WINBUILDER=1 ;;
    --submit)     SUBMIT=1 ;;
    *) echo "unknown option: $a"; exit 2 ;;
  esac
done

PKG="$(cd "$(dirname "$0")" && pwd)"
cd "$PKG"
VERSION=$(grep -m1 '^Version:' DESCRIPTION | sed 's/Version:[[:space:]]*//')
[ "$VERSION" = "1.6.0" ] || { echo "DESCRIPTION says version $VERSION, expected 1.6.0"; exit 1; }
SCRATCH="$(mktemp -d)"
TARBALL="oystermapR_${VERSION}.tar.gz"
say() { printf '\n=== %s\n' "$*"; }
echo "oystermapR $VERSION  |  scratch: $SCRATCH"

# ---- 1. documentation -------------------------------------------------------
# devtools::document(), fix_rd.py, and the reference manual PDF.
say "1/9  Regenerate documentation (make_docs.sh)"
bash make_docs.sh > "$SCRATCH/make_docs.log" 2>&1 || { tail -30 "$SCRATCH/make_docs.log"; exit 1; }
grep -E "man/|NAMESPACE" "$SCRATCH/make_docs.log"

# ---- 2. test suite ------------------------------------------------------------
say "2/9  Test suite (devtools::test)"
Rscript --vanilla -e '
  r <- as.data.frame(devtools::test(reporter = "silent"))
  cat(sprintf("%d tests: %d failed, %d errors, %d skipped\n",
              nrow(r), sum(r$failed), sum(r$error), sum(r$skipped)))
  if (any(r$failed > 0 | r$error)) { print(r[r$failed > 0 | r$error, c("file", "test")]); quit(status = 1) }
'

# ---- 3. build -----------------------------------------------------------------
# Pre-render the vignette exactly as cran_check.sh does, then build in scratch.
say "3/9  Build the tarball (in the scratch directory)"
Rscript --vanilla -e "
  vign <- file.path('$PKG', 'vignettes', 'example-bay-survey.Rmd')
  out  <- file.path('$PKG', 'inst', 'doc')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  rmarkdown::render(vign, output_format = rmarkdown::html_vignette(),
                    output_dir = out, quiet = TRUE)
  cat('vignette OK\n')" || echo "WARNING: vignette pre-render failed -- continuing"
( cd "$SCRATCH" && R CMD build "$PKG" > build.log 2>&1 ) || { tail -20 "$SCRATCH/build.log"; exit 1; }
[ -f "$SCRATCH/$TARBALL" ] || { echo "no tarball produced"; exit 1; }
echo "built $SCRATCH/$TARBALL ($(du -h "$SCRATCH/$TARBALL" | cut -f1))"
echo "top-level files in the tarball (expect DESCRIPTION NAMESPACE NEWS.md README.md):"
tar tzf "$SCRATCH/$TARBALL" | awk -F/ 'NF==2 && $2!="" {print "  " $2}'

# ---- 4. R CMD check --as-cran ---------------------------------------------------
say "4/9  R CMD check --as-cran   (several minutes)"
( cd "$SCRATCH" && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran "$TARBALL" > check.log 2>&1 ) || true
CHECKLOG="$SCRATCH/oystermapR.Rcheck/00check.log"
tail -3 "$SCRATCH/check.log"
STATUS=$(grep -E "^Status:" "$CHECKLOG" || echo "Status: (not found)")
echo "$STATUS"
if grep -qE "ERROR|WARNING" <<<"$STATUS"; then
  echo; grep -E -B1 -A8 "ERROR|WARNING" "$CHECKLOG" | head -60
  echo; echo "STOP: fix the errors/warnings above (full log: $CHECKLOG)"; exit 1
fi
if grep -qE "NOTE" <<<"$STATUS"; then
  echo; echo "NOTEs (compare with cran-comments.md -- CRAN reads them):"
  grep -E -A6 "NOTE" "$CHECKLOG"
fi

# ---- 5. clean-library install and smoke test --------------------------------------
say "5/9  Install into a clean library and run the walkthrough"
LIB="$SCRATCH/lib"; mkdir -p "$LIB"
R CMD INSTALL --library="$LIB" "$SCRATCH/$TARBALL" > "$SCRATCH/install.log" 2>&1 || { tail -20 "$SCRATCH/install.log"; exit 1; }
( cd "$SCRATCH" && cp "$PKG/lineage_walkthrough.R" . &&
  sed -i.bak 's|suppressMessages(devtools::load_all(".", quiet = TRUE))|.libPaths(c("'"$LIB"'", .libPaths())); suppressMessages(library(oystermapR))|' lineage_walkthrough.R &&
  Rscript lineage_walkthrough.R > walkthrough.log 2>&1 ) \
  && echo "walkthrough OK (installed package, no devtools)" \
  || { tail -25 "$SCRATCH/walkthrough.log"; exit 1; }

# ---- 6. version/NEWS/comments consistency --------------------------------------------
say "6/9  Consistency checks"
grep -q "^# oystermapR 1.6.0" NEWS.md            && echo "NEWS.md has a 1.6.0 section"      || { echo "NEWS.md is missing 1.6.0"; exit 1; }
grep -q "version 1.6.0" cran-comments.md          && echo "cran-comments.md names 1.6.0"      || { echo "cran-comments.md does not mention 1.6.0"; exit 1; }
# The win-builder line can only be filled in after --winbuilder has run, so a
# placeholder blocks --submit but not the earlier steps.
if grep -qi "FILL IN\|TODO\|XXX" cran-comments.md; then
  echo "cran-comments.md still has placeholders:"; grep -ni "FILL IN\|TODO\|XXX" cran-comments.md
  if [ "$SUBMIT" = 1 ]; then echo "STOP: fill these in before submitting."; exit 1
  else echo "(not blocking this run -- they must be filled in before --submit)"; fi
fi
echo "R CMD check status this run:  $STATUS"
echo "cran-comments.md says:        $(grep -m1 -E '^[0-9]+ errors' cran-comments.md || echo '(no summary line)')"

say "Local steps passed."
echo "Tarball for submission: $SCRATCH/$TARBALL"
echo "Read cran-comments.md once more: it must describe this run, and the summary line above must match."

# ---- 7. reverse dependencies ----------------------------------------------------------
say "7/9  Reverse dependencies"
Rscript --vanilla -e 'ap <- tools::package_dependencies("oystermapR", which = c("Depends","Imports","LinkingTo","Suggests"), reverse = TRUE, db = available.packages(repos = "https://cloud.r-project.org")); print(ap)' 2>/dev/null || echo "(could not query CRAN; cran-comments.md says none)"

# ---- 8. win-builder (uploads the package) -------------------------------------------------
if [ "$WINBUILDER" = 1 ]; then
  say "8/9  Upload to win-builder (R-release and R-devel); results are emailed to the maintainer"
  read -r -p "Upload $TARBALL to win-builder now? [y/N] " ok
  if [ "$ok" = "y" ]; then
    Rscript --vanilla -e "devtools::check_win_release('$PKG'); devtools::check_win_devel('$PKG')"
    echo "Uploaded. Wait for both emails and read them BEFORE submitting."
  else echo "skipped"; fi
else
  say "8/9  win-builder skipped (pass --winbuilder). cran-comments.md lists it as a test environment: run it first."
fi

# ---- 9. submit to CRAN --------------------------------------------------------------------------
if [ "$SUBMIT" = 1 ]; then
  say "9/9  Submit to CRAN"
  echo "This sends oystermapR $VERSION to CRAN's incoming queue. You will get a confirmation email"
  echo "to the maintainer address and must click its link. It cannot be undone."
  read -r -p "Type  SUBMIT 1.6.0  to continue: " phrase
  [ "$phrase" = "SUBMIT 1.6.0" ] || { echo "not submitted"; exit 0; }
  # submit_cran() asks its own yes/no questions (email address, comments reviewed), which
  # abort under Rscript because it is non-interactive. --interactive lets it read your answers.
  echo "devtools will now ask a few yes/no questions: answer them at the prompt."
  R --vanilla --quiet --interactive -e "devtools::submit_cran('$PKG')"
else
  say "9/9  Not submitted (pass --submit). Nothing has been sent to CRAN."
fi
