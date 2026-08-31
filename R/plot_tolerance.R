# =============================================================================
# Tolerance curve visualisation
# =============================================================================
#
# Draws the scoring function for any species / variable combination directly
# from the tolerance parameter spec -- no survey dataset required. Useful for:
#   - QA: verifying that threshold values match published literature
#   - Stakeholder communication: "here is exactly what the model rewards"
#   - Report figures: one panel per key variable
#
# For data-driven response curves (what the model actually did to a real
# survey), use sensitivity_analysis() in variable_importance.R instead.
# =============================================================================


#' Plot the tolerance scoring curve for a species and variable
#'
#' @description
#' Draws the suitability scoring function for any scored environmental variable
#' directly from the species tolerance specification -- no survey dataset is
#' required. The curve shows exactly what score the model assigns to each value
#' of the variable: 1 (optimal), declining gradients through acceptable and
#' poor zones, and 0 at hard limits.
#'
#' For a data-driven response curve derived from an actual survey (partial
#' dependence), use [sensitivity_analysis()] instead.
#'
#' @param species Character string. Species key (e.g. `"ostrea_edulis"`).
#'   Accepts the same formats as [predict_oyster()].
#' @param variable Character string. Name of the scored variable to plot
#'   (e.g. `"temperature"`, `"salinity"`, `"ph"`, `"omega_aragonite"`).
#'   Use `get_species_tolerances(species)$scored` to see available variables.
#' @param season Character or `NULL`. For seasonal variables (temperature),
#'   specifies which season's parameters to display. One of `"winter"`,
#'   `"spring"`, `"summer"`, `"autumn"`. If `NULL` (default), the base
#'   parameters are used; pass `"all"` to overlay all four seasons.
#' @param n_steps Integer. Number of points on the x-axis (default `500`).
#'   More = smoother curve.
#' @param show_exclusion Logical. If `TRUE` (default), overlays the hard
#'   exclusion threshold(s) from `tolerances$exclusions` as vertical red lines.
#' @param title Character or `NULL`. Plot title. `NULL` = auto-generated.
#' @param colour Character. Curve colour (default `"#2166ac"`, a marine blue).
#'
#' @return A `ggplot2` object (invisibly). Printed as a side effect. Can be
#'   further customised with standard `ggplot2` layers. Falls back to base R
#'   `plot()` if `ggplot2` is not installed.
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics barplot
#' @examples
#' # Temperature curve for O. edulis (base / year-round parameters)
#' plot_tolerance("ostrea_edulis", "temperature")
#'
#' # Summer temperature only
#' plot_tolerance("ostrea_edulis", "temperature", season = "summer")
#'
#' # All four seasons overlaid
#' plot_tolerance("ostrea_edulis", "temperature", season = "all")
#'
#' # pH curve for the European flat oyster
#' plot_tolerance("ostrea_edulis", "ph")
#'
#' # Aragonite saturation for Pacific oyster
#' plot_tolerance("magallana_gigas", "omega_aragonite")
#'
#' # Save to a temporary file
#' p <- plot_tolerance("ostrea_edulis", "salinity")
#' ggplot2::ggsave(file.path(tempdir(), "salinity_tolerance.png"), p, width = 8, height = 5)
plot_tolerance <- function(species,
                           variable,
                           season         = NULL,
                           n_steps        = 500L,
                           show_exclusion = TRUE,
                           title          = NULL,
                           colour         = "#2166ac") {

  # ---- Load tolerances --------------------------------------------------------
  tol    <- get_species_tolerances(species)
  scored <- tol$scored

  if (!variable %in% names(scored)) {
    cli::cli_abort(c(
      "Variable {.val {variable}} not found in scored block for {.val {species}}.",
      "i" = "Available variables: {.val {names(scored)}}"
    ))
  }

  base_params <- scored[[variable]]
  sp_label    <- tol$common_name %||% species
  unit        <- base_params$unit %||% ""

  # ---- Categorical variable: bar chart ----------------------------------------
  if (base_params$type == "categorical") {
    scores  <- base_params$scores
    df_cat  <- data.frame(category = names(scores), score = as.numeric(scores),
                          stringsAsFactors = FALSE)
    df_cat  <- df_cat[order(df_cat$score), ]
    df_cat$category <- factor(df_cat$category, levels = df_cat$category)

    auto_title <- title %||% paste0(sp_label, " -- ", variable, " tolerance")

    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot2::ggplot(df_cat,
               ggplot2::aes(x = category, y = score, fill = score)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_gradientn(
          colours = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
          limits  = c(0, 1), name = "Score"
        ) +
        ggplot2::labs(title = auto_title,
                      x = NULL, y = "Suitability score [0 - 1]") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::ylim(0, 1)
      print(p)
      return(invisible(p))
    }

    # Base R fallback
    pal <- colorRampPalette(c("#d73027", "#fee08b", "#1a9850"))(nrow(df_cat))
    barplot(df_cat$score, names.arg = df_cat$category, horiz = TRUE, las = 1,
            col = pal, xlim = c(0, 1), main = auto_title,
            xlab = "Suitability score [0-1]")
    return(invisible(df_cat))
  }

  # ---- Determine seasons to plot ----------------------------------------------
  season_override <- tol$seasonal_overrides[[variable]]
  all_seasons <- c("winter", "spring", "summer", "autumn")

  if (!is.null(season) && season == "all" && !is.null(season_override)) {
    season_list <- stats::setNames(all_seasons, all_seasons)
  } else if (!is.null(season) && season != "all" &&
             !is.null(season_override) && !is.null(season_override[[season]])) {
    season_list <- stats::setNames(season, season)
  } else {
    season_list <- stats::setNames("base", "base")
  }

  # ---- Determine global x range across all seasons ----------------------------
  x_range_list <- lapply(season_list, function(s) {
    p <- if (s == "base") base_params else (season_override[[s]] %||% base_params)
    .tolerance_x_range(p)
  })
  x_lo <- min(sapply(x_range_list, `[[`, "lo"))
  x_hi <- max(sapply(x_range_list, `[[`, "hi"))
  xs   <- seq(x_lo, x_hi, length.out = n_steps)

  # ---- Compute score curve(s) -------------------------------------------------
  df_list <- lapply(names(season_list), function(s) {
    p  <- if (s == "base") base_params else (season_override[[s]] %||% base_params)
    # seasonal sub-params use optimal_range scoring
    pp <- p
    if (!is.null(pp$type) && pp$type == "seasonal") pp$type <- "optimal_range"
    ys <- vapply(xs, function(x) .score_numeric(x, pp), numeric(1))
    data.frame(x = xs, score = ys,
               season = if (s == "base") "all seasons" else s,
               stringsAsFactors = FALSE)
  })
  df_lines <- do.call(rbind, df_list)

  # ---- Zone shading (from base params) ----------------------------------------
  bp <- base_params
  if (!is.null(bp$type) && bp$type == "seasonal") bp$type <- "optimal_range"
  zones  <- .tolerance_zones(bp, x_lo, x_hi)

  # ---- Auto title and x label -------------------------------------------------
  auto_title <- title %||% paste0(sp_label, " -- ", variable, " tolerance")
  x_label    <- if (nchar(unit) > 0) paste0(variable, "  [", unit, "]") else variable

  # ---- Exclusion lines --------------------------------------------------------
  excl_vals <- list()
  if (show_exclusion && !is.null(tol$exclusions[[variable]])) {
    ep <- tol$exclusions[[variable]]
    if (!is.null(ep$min))    excl_vals[["min"]]    <- ep$min
    if (!is.null(ep$max))    excl_vals[["max"]]    <- ep$max
  }

  # ---- ggplot2 path -----------------------------------------------------------
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    season_colours <- c(
      "all seasons" = colour,
      "winter"  = "#4575b4",
      "spring"  = "#74c476",
      "summer"  = "#d73027",
      "autumn"  = "#f46d43",
      "base"    = colour
    )

    p <- ggplot2::ggplot()

    # Background zone rectangles
    for (z in zones) {
      p <- p + ggplot2::annotate("rect",
               xmin = z$xmin, xmax = z$xmax,
               ymin = 0,      ymax = 1,
               fill = z$fill, alpha = 0.12)
    }

    # Score line(s)
    if (length(unique(df_lines$season)) == 1) {
      p <- p + ggplot2::geom_line(data = df_lines,
                 ggplot2::aes(x = x, y = score),
                 colour = colour, linewidth = 1.1)
      p <- p + ggplot2::geom_ribbon(data = df_lines,
                 ggplot2::aes(x = x, ymin = 0, ymax = score),
                 fill = colour, alpha = 0.15)
    } else {
      p <- p + ggplot2::geom_line(data = df_lines,
                 ggplot2::aes(x = x, y = score, colour = season),
                 linewidth = 1.0) +
        ggplot2::scale_colour_manual(
          values = season_colours[unique(df_lines$season)],
          name = "Season"
        )
    }

    # Exclusion threshold lines
    for (nm in names(excl_vals)) {
      v <- excl_vals[[nm]]
      if (v >= x_lo && v <= x_hi) {
        p <- p + ggplot2::geom_vline(xintercept = v,
               colour = "#c0392b", linetype = "dashed", linewidth = 0.8) +
          ggplot2::annotate("text", x = v, y = 0.05,
               label = paste0("excl. ", nm, " (", v, ")"),
               hjust = -0.05, size = 3, colour = "#c0392b")
      }
    }

    # Zone labels at top
    for (z in zones) {
      mid_x <- (z$xmin + z$xmax) / 2
      if (mid_x >= x_lo && mid_x <= x_hi && nchar(z$label) > 0) {
        p <- p + ggplot2::annotate("text",
               x = mid_x, y = 0.97,
               label = z$label, size = 2.8,
               colour = "#444444", alpha = 0.8)
      }
    }

    p <- p +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                  breaks = seq(0, 1, 0.2),
                                  expand = ggplot2::expansion(mult = c(0, 0.03))) +
      ggplot2::labs(title = auto_title,
                    x = x_label,
                    y = "Suitability score [0 - 1]") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )

    print(p)
    return(invisible(p))
  }

  # ---- Base R fallback --------------------------------------------------------
  plot(df_lines$x[df_lines$season == df_lines$season[1]],
       df_lines$score[df_lines$season == df_lines$season[1]],
       type = "l", col = colour, lwd = 2,
       xlab = x_label, ylab = "Suitability score [0-1]",
       ylim = c(0, 1), main = auto_title)
  if (length(unique(df_lines$season)) > 1) {
    sc_cols <- c("#4575b4","#74c476","#d73027","#f46d43")
    for (i in seq_along(all_seasons)) {
      sub <- df_lines[df_lines$season == all_seasons[i], ]
      if (nrow(sub) > 0)
        graphics::lines(sub$x, sub$score, col = sc_cols[i], lwd = 1.5)
    }
    graphics::legend("topright", legend = all_seasons,
                     col = sc_cols, lwd = 1.5, cex = 0.8)
  }
  for (v in excl_vals) {
    graphics::abline(v = v, col = "#c0392b", lty = 2)
  }

  invisible(df_lines)
}


# Suppress R CMD check NOTEs for ggplot2 aes() column name variables
utils::globalVariables(c("category", "x", "score", "season"))

# ---- Internal helpers ---------------------------------------------------------

#' @keywords internal
.tolerance_x_range <- function(params) {
  type <- params$type %||% "optimal_range"
  if (type %in% c("optimal_range", "seasonal")) {
    lo_raw <- params$poor_min %||% params$acceptable_min %||%
              (params$optimal_min - (params$optimal_max - params$optimal_min))
    hi_raw <- params$absolute_max %||% params$acceptable_max %||%
              (params$optimal_max + (params$optimal_max - params$optimal_min))
  } else {
    lo_raw <- 0
    hi_raw <- params$hard_max %||% params$poor_max %||% (params$optimal_max * 2)
  }
  pad <- (hi_raw - lo_raw) * 0.12
  list(lo = lo_raw - pad, hi = hi_raw + pad)
}


#' @keywords internal
.tolerance_zones <- function(params, x_lo, x_hi) {
  type <- params$type %||% "optimal_range"

  if (type %in% c("optimal_range", "seasonal")) {
    lo      <- params$poor_min %||% params$acceptable_min %||%
               (params$optimal_min - (params$optimal_max - params$optimal_min))
    hi      <- params$absolute_max %||% params$acceptable_max %||%
               (params$optimal_max + (params$optimal_max - params$optimal_min))
    opt_min <- params$optimal_min
    opt_max <- params$optimal_max

    list(
      list(xmin = x_lo,    xmax = lo,      fill = "#d73027", label = "excluded"),
      list(xmin = lo,      xmax = opt_min, fill = "#fc8d59", label = "poor -> acceptable"),
      list(xmin = opt_min, xmax = opt_max, fill = "#1a9850", label = "optimal"),
      list(xmin = opt_max, xmax = hi,      fill = "#fc8d59", label = "acceptable -> poor"),
      list(xmin = hi,      xmax = x_hi,   fill = "#d73027", label = "excluded")
    )
  } else {
    # threshold_decay
    opt_max  <- params$optimal_max
    hard_max <- params$hard_max %||% params$poor_max %||% (opt_max * 2)
    list(
      list(xmin = x_lo,    xmax = opt_max,  fill = "#1a9850", label = "optimal"),
      list(xmin = opt_max, xmax = hard_max, fill = "#fc8d59", label = "declining"),
      list(xmin = hard_max, xmax = x_hi,   fill = "#d73027", label = "excluded")
    )
  }
}
