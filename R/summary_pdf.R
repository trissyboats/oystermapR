# =============================================================================
# generate_summary_pdf() -- printer-friendly multi-page suitability summary
# No LaTeX, no pandoc -- uses grDevices::pdf() + ggplot2 only.
# =============================================================================

utils::globalVariables(c(
  "score", "bin_class", "variable", "class", "n",
  "suitability", "suitability_class", "lon", "lat"
))

#' Generate a compact printable PDF summary of oystermapR results
#'
#' @description
#' Produces a self-contained multi-page A4 PDF directly from the output of
#' [predict_oyster()] without requiring LaTeX, pandoc, or Rmd rendering.
#' The PDF is designed for printing and contains four pages:
#'
#' * **Page 1** -- Title header, executive summary table, class breakdown bar
#' * **Page 2** -- Suitability heatmap (survey points, continuous colour ramp)
#' * **Page 3** -- Per-variable mean score chart (horizontal bar)
#' * **Page 4** -- Score distribution histogram + class count bar chart
#'
#' @param result A dataframe returned by [predict_oyster()].
#' @param output Character. Output `.pdf` file path.
#'   Default `"oyster_summary.pdf"`.
#' @param species Character. Species key used in [predict_oyster()].
#'   Default `"ostrea_edulis"`.
#' @param title Character. Report title shown in the header.
#'   Default `"Oyster Suitability Summary"`.
#' @param author Character. Author name (optional, shown in header).
#' @param open Logical. Open the PDF after writing (default `TRUE`).
#' @param verbose Logical. Print progress messages (default `TRUE`).
#'
#' @return The output file path (invisibly).
#'
#' @export
#' @examples
#' sample_csv <- system.file("extdata", "sample_survey.csv", package = "oystermapR")
#' result <- predict_oyster(sample_csv, "ostrea_edulis", verbose = FALSE)
#' out_pdf <- file.path(tempdir(), "kames_bay_summary.pdf")
#' generate_summary_pdf(result, out_pdf,
#'                      title  = "Kames Bay -- Spring Survey",
#'                      author = "T. Tucker",
#'                      open   = FALSE)
generate_summary_pdf <- function(result,
                                  output  = "oyster_summary.pdf",
                                  species = "ostrea_edulis",
                                  title   = "Oyster Suitability Summary",
                                  author  = "",
                                  open    = TRUE,
                                  verbose = TRUE) {

  # -- Dependencies ---------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE))
    cli::cli_abort(c(
      "Package {.pkg ggplot2} is required by {.fn generate_summary_pdf}.",
      "i" = "Install it with: {.code install.packages('ggplot2')}"
    ))

  # -- Tolerances -----------------------------------------------------------
  tol <- get_species_tolerances(species)

  # -- Guard columns --------------------------------------------------------
  if (!"excluded" %in% names(result)) result$excluded <- FALSE
  result$excluded <- as.logical(result$excluded)
  result$excluded[is.na(result$excluded)] <- FALSE

  if (!"suitability_class" %in% names(result)) {
    result$suitability_class <- ifelse(
      is.na(result$suitability), "Excluded",
      ifelse(result$suitability >= 0.70, "High",
      ifelse(result$suitability >= 0.45, "Moderate",
      ifelse(result$suitability >= 0.20, "Low", "Very Low")))
    )
  }

  # -- Summary statistics ----------------------------------------------------
  n_total  <- nrow(result)
  n_excl   <- sum(result$excluded, na.rm = TRUE)
  n_scored <- n_total - n_excl
  scored   <- result[!result$excluded & !is.na(result$suitability), ]
  n_high   <- sum(scored$suitability_class == "High",     na.rm = TRUE)
  n_mod    <- sum(scored$suitability_class == "Moderate", na.rm = TRUE)
  n_low    <- sum(scored$suitability_class == "Low",      na.rm = TRUE)
  n_vlow   <- sum(scored$suitability_class == "Very Low", na.rm = TRUE)
  mean_s   <- round(mean(scored$suitability, na.rm = TRUE), 3)

  CLASS_COL <- c(
    "High"     = "#27ae60",
    "Moderate" = "#e67e22",
    "Low"      = "#e74c3c",
    "Very Low" = "#c0392b",
    "Excluded" = "#95a5a6"
  )

  # -- Ensure output directory -----------------------------------------------
  out_dir <- dirname(output)
  if (!dir.exists(out_dir) && nchar(out_dir) > 0 && out_dir != ".")
    dir.create(out_dir, recursive = TRUE)

  if (verbose) cli::cli_inform("Generating print summary PDF: {.file {output}}...")

  # -- Open PDF device (A4 portrait, no LaTeX) --------------------------------
  grDevices::pdf(file = normalizePath(output, mustWork = FALSE),
                 width = 8.27, height = 11.69,
                 title = title, paper = "a4")
  on.exit({
    grDevices::dev.off()
    if (verbose && file.exists(output))
      cli::cli_inform(c("v" = "PDF written: {.file {output}}"))
  }, add = TRUE)

  # ===========================================================================
  # PAGE 1 -- Executive summary
  # ===========================================================================
  grid::grid.newpage()

  # -- Header bar -------------------------------------------------------------
  grid::grid.rect(x = 0.5, y = 0.970, width = 1, height = 0.060,
                  gp = grid::gpar(fill = "#1a6b6b", col = NA))
  grid::grid.text(title, x = 0.04, y = 0.970, just = "left",
                  gp = grid::gpar(fontsize = 15, fontface = "bold", col = "white"))

  author_str <- if (nchar(trimws(author)) > 0) paste0("  \u00b7  ", author) else ""
  grid::grid.text(
    paste0(tol$latin_name, "  \u2014  ", tol$common_name, author_str,
           "  \u00b7  ", format(Sys.Date(), "%d %B %Y")),
    x = 0.04, y = 0.928, just = "left",
    gp = grid::gpar(fontsize = 8.5, col = "#6c7a89"))

  # -- Thin rule --------------------------------------------------------------
  grid::grid.lines(x = c(0.04, 0.96), y = c(0.910, 0.910),
                   gp = grid::gpar(col = "#dde3e8", lwd = 0.8))

  # -- Summary table ----------------------------------------------------------
  stat_df <- data.frame(
    Metric = c("Survey locations",
               "Excluded (hard limits)",
               "Scored locations",
               "Mean suitability (scored)",
               paste0("High (\u2265 0.70)"),
               "Moderate (0.45\u20130.70)",
               "Low (0.20\u20130.45)",
               "Very Low (< 0.20)"),
    Value  = as.character(c(n_total, n_excl, n_scored, mean_s,
                             n_high, n_mod, n_low, n_vlow)),
    stringsAsFactors = FALSE
  )

  row_h   <- 0.037
  col1_x  <- 0.07
  col2_x  <- 0.93
  start_y <- 0.893

  # Header row
  grid::grid.rect(x = 0.5, y = start_y + row_h * 0.5, width = 0.90, height = row_h,
                  gp = grid::gpar(fill = "#2c3e50", col = NA))
  grid::grid.text("Metric", x = col1_x, y = start_y + row_h * 0.5, just = "left",
                  gp = grid::gpar(fontsize = 8.5, fontface = "bold", col = "white"))
  grid::grid.text("Value",  x = col2_x, y = start_y + row_h * 0.5, just = "right",
                  gp = grid::gpar(fontsize = 8.5, fontface = "bold", col = "white"))

  for (i in seq_len(nrow(stat_df))) {
    ypos <- start_y - i * row_h
    bg   <- if (i %% 2 == 0) "#fafbfc" else "white"
    grid::grid.rect(x = 0.5, y = ypos + row_h * 0.5, width = 0.90, height = row_h,
                    gp = grid::gpar(fill = bg, col = "#dde3e8", lwd = 0.5))
    grid::grid.text(stat_df$Metric[i], x = col1_x, y = ypos + row_h * 0.5, just = "left",
                    gp = grid::gpar(fontsize = 8.5, col = "#2c3e50"))
    grid::grid.text(stat_df$Value[i],  x = col2_x, y = ypos + row_h * 0.5, just = "right",
                    gp = grid::gpar(fontsize = 8.5, col = "#2c3e50"))
  }

  # -- Suitability class breakdown bar ---------------------------------------
  bar_top <- start_y - (nrow(stat_df) + 1.4) * row_h
  classes <- c("High", "Moderate", "Low", "Very Low")
  counts  <- c(n_high, n_mod, n_low, n_vlow)
  total_c <- sum(counts)

  if (total_c > 0) {
    fracs   <- counts / total_c
    cum_f   <- c(0, cumsum(fracs))
    bx0     <- 0.07
    bw      <- 0.86
    bh      <- 0.028

    grid::grid.text("Suitability class breakdown (scored locations)",
                    x = bx0, y = bar_top + bh + 0.018, just = "left",
                    gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#2c3e50"))

    for (j in seq_along(classes)) {
      if (fracs[j] < 0.001) next
      mid_x <- bx0 + (cum_f[j] + fracs[j] / 2) * bw
      grid::grid.rect(x = mid_x, y = bar_top, width = fracs[j] * bw, height = bh,
                      gp = grid::gpar(fill = CLASS_COL[classes[j]], col = NA))
      if (fracs[j] > 0.07) {
        grid::grid.text(
          paste0(classes[j], "\n", round(fracs[j] * 100), "%"),
          x = mid_x, y = bar_top,
          gp = grid::gpar(fontsize = 7, col = "white", fontface = "bold",
                          lineheight = 1.1))
      }
    }
  }

  # -- Footer -----------------------------------------------------------------
  grid::grid.rect(x = 0.5, y = 0.018, width = 1, height = 0.036,
                  gp = grid::gpar(fill = "#2c3e50", col = NA))
  grid::grid.text(
    paste0("oystermapR ", utils::packageVersion("oystermapR"),
           "  \u00b7  ", tol$latin_name,
           "  \u00b7  Data quality: ", tol$data_quality,
           "  \u00b7  Page 1 of 4"),
    x = 0.5, y = 0.018,
    gp = grid::gpar(fontsize = 7, col = grDevices::rgb(1, 1, 1, 0.7)))

  # ===========================================================================
  # PAGE 2 -- Suitability heatmap
  # ===========================================================================
  has_coords <- all(c("lat", "lon") %in% names(result)) &&
                sum(!is.na(result$lat) & !is.na(result$lon)) > 0

  if (has_coords) {
    map_df <- result[!is.na(result$lat) & !is.na(result$lon), ]
    scored_map <- map_df[!map_df$excluded & !is.na(map_df$suitability), ]

    lat_range <- range(map_df$lat, na.rm = TRUE)
    lon_range <- range(map_df$lon, na.rm = TRUE)
    # Aspect ratio: 1 lon deg ~ cos(lat) x 1 lat deg
    aspect <- 1 / cos(mean(lat_range) * pi / 180)

    n_pts  <- nrow(scored_map)
    n_bins <- min(40, max(8, round(sqrt(n_pts) * 1.5)))

    p_heat <- ggplot2::ggplot(scored_map,
                              ggplot2::aes(x = lon, y = lat)) +
      # Continuous heatmap surface via 2D binning
      ggplot2::stat_summary_2d(
        ggplot2::aes(z = suitability),
        bins  = n_bins,
        fun   = mean,
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_gradientn(
        colours = c("#c0392b", "#e74c3c", "#e67e22", "#f1c40f", "#27ae60"),
        values  = c(0, 0.20, 0.45, 0.65, 1),
        limits  = c(0, 1),
        name    = "Mean\nSuitability",
        breaks  = c(0, 0.20, 0.45, 0.70, 1.0),
        labels  = c("0.0\nVery Low", "0.2\nLow", "0.45\nModerate", "0.70\nHigh", "1.0"),
        guide   = ggplot2::guide_colorbar(barheight = 10, barwidth = 0.8,
                                          ticks = TRUE, ticks.colour = "white")
      ) +
      # Survey point overlay
      ggplot2::geom_point(
        ggplot2::aes(colour = suitability_class),
        size = 2.5, alpha = 0.85, shape = 21,
        fill = NA, stroke = 0.7
      ) +
      ggplot2::scale_colour_manual(
        values = CLASS_COL,
        name   = "Class",
        breaks = c("High", "Moderate", "Low", "Very Low", "Excluded")
      ) +
      # Excluded points
      {if (sum(map_df$excluded) > 0)
        ggplot2::geom_point(
          data   = map_df[map_df$excluded, ],
          ggplot2::aes(x = lon, y = lat),
          colour = "#95a5a6", size = 2, shape = 4, stroke = 0.8
        )
      } +
      ggplot2::coord_fixed(ratio = aspect) +
      ggplot2::labs(
        title    = paste0(tol$common_name, " \u2014 Suitability Heatmap"),
        subtitle = paste0(n_pts, " scored locations  \u00b7  ",
                          format(Sys.Date(), "%d %B %Y"),
                          "  \u00b7  Colour = mean suitability per grid cell"),
        x = "Longitude (\u00b0E)",
        y = "Latitude (\u00b0N)",
        caption = paste0("Species: ", tol$latin_name,
                         "  \u00b7  oystermapR ", utils::packageVersion("oystermapR"))
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold", colour = "#1a6b6b",
                                                size = 14, margin = ggplot2::margin(b = 4)),
        plot.subtitle   = ggplot2::element_text(colour = "#6c7a89", size = 8),
        plot.caption    = ggplot2::element_text(colour = "#aaa", size = 7),
        legend.position = "right",
        panel.background = ggplot2::element_rect(fill = "#eaf2f8", colour = NA),
        panel.grid.major = ggplot2::element_line(colour = "white", linewidth = 0.4),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text       = ggplot2::element_text(size = 8),
        plot.margin     = ggplot2::margin(12, 12, 12, 12)
      )

    print(p_heat)
  } else {
    grid::grid.newpage()
    grid::grid.text("No coordinate data available for heatmap.",
                    gp = grid::gpar(fontsize = 12, col = "#6c7a89"))
  }

  # ===========================================================================
  # PAGE 3 -- Variable scores
  # ===========================================================================
  score_cols <- grep("^score_", names(result), value = TRUE)

  if (length(score_cols) > 0) {
    non_excl  <- result[!result$excluded, ]
    var_names <- gsub("^score_", "", score_cols)
    var_means <- vapply(score_cols,
                        function(cn) round(mean(non_excl[[cn]], na.rm = TRUE), 3),
                        numeric(1))
    names(var_means) <- var_names
    var_df <- data.frame(
      variable = factor(names(var_means),
                        levels = names(sort(var_means))),
      score    = as.numeric(var_means),
      stringsAsFactors = FALSE
    )
    var_df$class <- ifelse(var_df$score >= 0.70, "High",
                    ifelse(var_df$score >= 0.45, "Moderate", "Low"))

    p_vars <- ggplot2::ggplot(var_df,
                ggplot2::aes(x = score, y = variable, fill = class)) +
      ggplot2::geom_col(alpha = 0.82, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", score)),
                         hjust = -0.12, size = 2.9, colour = "#2c3e50") +
      ggplot2::scale_fill_manual(
        values = c("High" = "#27ae60", "Moderate" = "#e67e22", "Low" = "#e74c3c"),
        guide  = "none"
      ) +
      ggplot2::geom_vline(xintercept = 0.45,
                          linetype = "dashed", colour = "#bbb", linewidth = 0.5) +
      ggplot2::geom_vline(xintercept = 0.70,
                          linetype = "dashed", colour = "#999", linewidth = 0.5) +
      ggplot2::annotate("text", x = 0.45, y = Inf, vjust = -0.4, hjust = -0.1,
                        label = "0.45", size = 2.5, colour = "#aaa") +
      ggplot2::annotate("text", x = 0.70, y = Inf, vjust = -0.4, hjust = -0.1,
                        label = "0.70", size = 2.5, colour = "#aaa") +
      ggplot2::scale_x_continuous(limits = c(0, 1.15),
                                  breaks = c(0, 0.2, 0.45, 0.7, 1.0)) +
      ggplot2::labs(
        title    = "Per-Variable Mean Scores",
        subtitle = "Non-excluded locations only  \u00b7  dashed lines = class thresholds",
        x        = "Mean Score (0 \u2013 1)",
        y        = NULL,
        caption  = paste0("Species: ", tol$latin_name,
                           "  \u00b7  oystermapR ", utils::packageVersion("oystermapR"))
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold", colour = "#1a6b6b",
                                                   size = 14, margin = ggplot2::margin(b = 4)),
        plot.subtitle      = ggplot2::element_text(colour = "#6c7a89", size = 8),
        plot.caption       = ggplot2::element_text(colour = "#aaa", size = 7),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        axis.text.y        = ggplot2::element_text(size = 9),
        plot.margin        = ggplot2::margin(12, 12, 12, 12)
      )

    print(p_vars)
  }

  # ===========================================================================
  # PAGE 4 -- Score distribution + class bar
  # ===========================================================================
  if (nrow(scored) > 0) {
    scored$bin_class <- ifelse(scored$suitability >= 0.70, "High",
                        ifelse(scored$suitability >= 0.45, "Moderate",
                        ifelse(scored$suitability >= 0.20, "Low", "Very Low")))
    scored$bin_class <- factor(scored$bin_class,
                               levels = c("High", "Moderate", "Low", "Very Low"))

    p_hist <- ggplot2::ggplot(scored,
                ggplot2::aes(x = suitability, fill = bin_class)) +
      ggplot2::geom_histogram(bins = 20, colour = "white", linewidth = 0.3) +
      ggplot2::scale_fill_manual(
        values = CLASS_COL[c("High", "Moderate", "Low", "Very Low")],
        name   = "Class",
        drop   = FALSE
      ) +
      ggplot2::geom_vline(xintercept = mean_s,
                          linetype = "dotted", colour = "#2c3e50", linewidth = 0.8) +
      ggplot2::annotate("text", x = mean_s + 0.01, y = Inf, vjust = 1.6, hjust = 0,
                        label = paste0("Mean: ", mean_s),
                        size = 3, colour = "#2c3e50") +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  breaks = c(0, 0.2, 0.45, 0.7, 1.0)) +
      ggplot2::labs(
        title    = "Score Distribution",
        subtitle = paste0(n_scored, " scored locations"),
        x        = "Suitability Score",
        y        = "Number of locations"
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", colour = "#1a6b6b", size = 13),
        plot.subtitle = ggplot2::element_text(colour = "#6c7a89", size = 8),
        legend.position = "top",
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(12, 12, 6, 12)
      )

    class_df <- data.frame(
      class = factor(c("High", "Moderate", "Low", "Very Low", "Excluded"),
                     levels = rev(c("High", "Moderate", "Low", "Very Low", "Excluded"))),
      n     = c(n_high, n_mod, n_low, n_vlow, n_excl)
    )

    p_class <- ggplot2::ggplot(class_df,
                 ggplot2::aes(x = n, y = class, fill = class)) +
      ggplot2::geom_col(alpha = 0.82, width = 0.65) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(n, "  (", round(n / n_total * 100, 1), "%)")),
        hjust = -0.05, size = 3, colour = "#2c3e50"
      ) +
      ggplot2::scale_fill_manual(
        values = CLASS_COL[levels(class_df$class)],
        guide  = "none"
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.22))
      ) +
      ggplot2::labs(
        title   = "Class Breakdown",
        x       = "Locations",
        y       = NULL,
        caption = paste0("Species: ", tol$latin_name,
                          "  \u00b7  oystermapR ", utils::packageVersion("oystermapR"))
      ) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold", colour = "#1a6b6b", size = 13),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        plot.caption       = ggplot2::element_text(colour = "#aaa", size = 7),
        plot.margin        = ggplot2::margin(6, 12, 12, 12)
      )

    # Stack both plots on one page
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(2, 1, heights = c(0.54, 0.46)))
    )
    print(p_hist,  vp = grid::viewport(layout.pos.row = 1))
    print(p_class, vp = grid::viewport(layout.pos.row = 2))
    grid::popViewport()
  }

  # -- Open in viewer ------------------------------------------------------
  if (open && file.exists(output)) {
    tryCatch(
      utils::browseURL(paste0("file://", normalizePath(output, mustWork = FALSE))),
      error = function(e) NULL
    )
  }

  invisible(output)
}
