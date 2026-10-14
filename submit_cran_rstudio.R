# Run this in RStudio (it needs an interactive console). Step through with Cmd+Enter.
#
# Win-builder keeps results for only about 12 hours, and a clean run from last night is
# not evidence about the tarball you submit today. So this script
#   PART A uploads the current package to win-builder (R-release and R-devel) and records
#          when, then
#   PART B refuses to submit unless that upload is under 12 hours old.
# Do PART A, wait for both emails to arrive and read them, then do PART B in the same day.
#
# Before PART B: search your inbox AND spam for "CRAN submission oystermapR <version>".
# If that email already exists, click its link instead; do not submit twice.

pkg   <- "/Users/home/Documents/Claude/Projects/oystermapR"
stamp <- normalizePath(file.path(path.expand("~"), ".oystermapR_winbuilder_sent"), mustWork = FALSE)
max_age_hours <- 12

d <- read.dcf(file.path(pkg, "DESCRIPTION"), fields = c("Package", "Version", "Maintainer"))
print(d)   # expect oystermapR, the version you mean to release, and your address

# ---- PART A: win-builder -------------------------------------------------------------
# Uploads the package and emails the results to the Maintainer address (about 15-30 min).
devtools::check_win_release(pkg)
devtools::check_win_devel(pkg)
writeLines(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), d[1, "Version"]), stamp)
message("Uploaded. Now wait for BOTH emails and read them. Continue only if each shows ",
        "no ERRORs, no WARNINGs and no NOTEs beyond those in cran-comments.md.")

# ---- PART B: submit (run only after PART A's emails are clean) --------------------------
# Refuses if the win-builder upload is missing, is for a different version, or is too old.
chk <- if (file.exists(stamp)) readLines(stamp) else character()
if (length(chk) < 2) stop("No win-builder upload recorded: run PART A first.")
sent <- as.POSIXct(chk[1], format = "%Y-%m-%d %H:%M:%S %z")
age  <- as.numeric(difftime(Sys.time(), sent, units = "hours"))
if (chk[2] != d[1, "Version"])
  stop("The recorded win-builder run was for version ", chk[2], ", not ", d[1, "Version"], ".")
if (is.na(age) || age > max_age_hours)
  stop(sprintf("The win-builder run is %.1f hours old (limit %d). Re-run PART A.",
               age, max_age_hours))
message(sprintf("win-builder upload is %.1f hours old: OK.", age))

# devtools builds the tarball, then asks numbered questions in the console:
# answer with the number of the Yes option and press Enter each time.
devtools::submit_cran(pkg)

# Afterwards CRAN emails a confirmation link to the maintainer address (check spam).
# The submission only counts after you click it.
