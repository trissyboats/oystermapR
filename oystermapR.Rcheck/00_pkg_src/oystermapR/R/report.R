# =============================================================================
# PDF / HTML Report Generation for oystermapR
# =============================================================================

#' Generate a formatted survey report from predict_oyster() results
#'
#' @description
#' Renders a structured assessment report from the output of [predict_oyster()].
#' The report includes an executive summary, suitability class breakdown,
#' per-variable scoring table, most common limiting factors, top introduction
#' sites, and a methodology section.
#'
#' Output format is PDF by default (requires a LaTeX installation \u2014 see Note
#' below). HTML is also supported and requires no additional system software.
#'
#' @param result A dataframe returned by [predict_oyster()].
#' @param output Character. Output file path. Extension determines format:
#'   `.pdf` for PDF, `.html` for HTML. Default `"oyster_report.pdf"`.
#' @param title Character. Report title. Default `"Oyster Suitability Assessment"`.
#' @param author Character. Author name(s) to appear on the title page.
#'   Default `""` (omitted).
#' @param species Character. Species key used in [predict_oyster()].
#'   Default `"ostrea_edulis"`.
#' @param top_n Integer. Number of top introduction sites to include in the
#'   report (default `5`).
#' @param validation Named list or `NULL`. Optional output from
#'   [validate_against_records()]. When supplied, adds a Validation section
#'   with ROC curve, AUC, TSS, and confusion-matrix metrics.
#' @param comparison Dataframe or `NULL`. Optional output from
#'   [compare_species()]. When supplied, adds a Competition Adjustment section
#'   showing the *M. gigas* competitive pressure penalty on *O. edulis* scores.
#' @param open Logical. Open the report in your default viewer after rendering
#'   (default `TRUE`).
#'
#' @note **HTML reports** (recommended) require `plotly` and `scales`:
#'   `install.packages(c("plotly","scales"))`.
#'   **PDF reports** additionally require a LaTeX installation; see the
#'   Note section below.
#'
#' @return The output file path (invisibly).
#'
#' @note **PDF output requires LaTeX.** If you don't have LaTeX installed,
#'   either use `output = "report.html"` (no extra software needed), or install
#'   a minimal LaTeX distribution with:
#'   ```r
#'   install.packages("tinytex")
#'   tinytex::install_tinytex()
#'   ```
#'
#' @export
#' @examples
#' \dontrun{
#' result <- predict_oyster(survey, "ostrea_edulis",
#'                          output_geotiff = "kames_bay.tif")
#'
#' # Standard HTML report (recommended \u2014 Plotly charts, no LaTeX needed)
#' generate_report(result, "kames_bay_report.html",
#'                 title  = "Kames Bay Oyster Habitat Assessment",
#'                 author = "T. Tucker")
#'
#' # With validation results embedded
#' val  <- validate_against_records(result, nbn_records)
#' generate_report(result, "kames_bay_validated.html",
#'                 validation = val)
#'
#' # With competition adjustment (when comparing species)
#' comp <- compare_species(survey,
#'           species = c("ostrea_edulis", "magallana_gigas"))
#' generate_report(result, "kames_bay_competition.html",
#'                 comparison = comp)
#'
#' # PDF report (requires LaTeX / tinytex)
#' generate_report(result, "kames_bay_report.pdf",
#'                 title = "Kames Bay Oyster Habitat Assessment")
#' }
generate_report <- function(result,
                            output     = "oyster_report.html",
                            title      = "Oyster Suitability Assessment",
                            author     = "",
                            species    = "ostrea_edulis",
                            top_n      = 5L,
                            validation = NULL,
                            comparison = NULL,
                            open       = TRUE) {

  # ---- Auto-install missing dependencies -------------------------------------
  .ensure_pkg <- function(pkg, reason = NULL) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      msg <- if (!is.null(reason)) reason else
        paste0("Required for generate_report(): installing {", pkg, "} now.")
      cli::cli_inform(paste0("i" = msg))
      utils::install.packages(pkg, quiet = TRUE)
      if (!requireNamespace(pkg, quietly = TRUE)) {
        cli::cli_abort(c(
          "Could not install package {.pkg {pkg}}.",
          "i" = "Please run: {.code install.packages('{pkg}')} manually."
        ))
      }
      cli::cli_inform(paste0("\u2713 ", pkg, " installed successfully."))
    }
  }

  .ensure_pkg("rmarkdown", "Package rmarkdown is required to render reports.")
  .ensure_pkg("knitr",     "Package knitr is required to render reports.")

  # HTML reports use Plotly for interactive charts
  ext_check <- tolower(tools::file_ext(output))
  if (ext_check == "html") {
    .ensure_pkg("plotly",     "Package plotly is required for interactive HTML charts.")
    .ensure_pkg("scales",     "Package scales is required for HTML colour mapping.")
    .ensure_pkg("htmlwidgets","Package htmlwidgets is required to embed Plotly in HTML.")
    .ensure_pkg("leaflet",    "Package leaflet is required for the interactive spatial map.")
  }

  # ---- Resolve format --------------------------------------------------------
  ext <- tolower(tools::file_ext(output))
  if (!ext %in% c("pdf", "html")) {
    cli::cli_abort(c(
      "Output file extension must be {.val pdf} or {.val html}.",
      "x" = "Got: {.val {ext}}"
    ))
  }

  if (ext == "pdf" && !requireNamespace("tinytex", quietly = TRUE)) {
    has_latex <- nchar(Sys.which("pdflatex")) > 0 ||
                 nchar(Sys.which("xelatex")) > 0  ||
                 nchar(Sys.which("lualatex")) > 0
    if (!has_latex) {
      cli::cli_warn(c(
        "!" = "No LaTeX installation detected. PDF rendering may fail.",
        "i" = "Install TinyTeX with: {.code tinytex::install_tinytex()}",
        "i" = "Or use HTML output: change extension to {.val .html}"
      ))
    }
  }

  # ---- Locate template -------------------------------------------------------
  template_path <- system.file("rmd", "oystermapR_report.Rmd",
                               package = "oystermapR")
  if (!nzchar(template_path) || !file.exists(template_path)) {
    cli::cli_abort(c(
      "Report template not found.",
      "i" = "Reinstall oystermapR: {.code devtools::install('path/to/oystermapR')}"
    ))
  }

  # ---- Set output format -----------------------------------------------------
  out_format <- if (ext == "pdf") "pdf_document" else "html_document"

  # ---- Ensure output directory exists ----------------------------------------
  out_dir <- dirname(output)
  if (!dir.exists(out_dir) && out_dir != ".") dir.create(out_dir, recursive = TRUE)

  cli::cli_inform("Generating {toupper(ext)} report: {.file {output}}...")

  # ---- Render ----------------------------------------------------------------
  rmarkdown::render(
    input         = template_path,
    output_format = out_format,
    output_file   = normalizePath(output, mustWork = FALSE),
    params        = list(
      result      = result,
      title       = title,
      author      = author,
      species_key = species,
      top_n       = top_n,
      validation  = validation,
      comparison  = comparison
    ),
    envir   = new.env(parent = globalenv()),
    quiet   = TRUE
  )

  cli::cli_inform(c(
    "v" = "Report written: {.file {output}}"
  ))

  if (open && file.exists(output)) {
    abs_path <- normalizePath(output, mustWork = FALSE)
    tryCatch(
      utils::browseURL(paste0("file://", abs_path)),
      error = function(e) cli::cli_inform("Report saved. Open manually: {.file {abs_path}}")
    )
  }

  invisible(output)
}
