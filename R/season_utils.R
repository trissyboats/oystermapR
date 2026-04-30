#' Detect meteorological season from date and latitude
#'
#' @description
#' Returns the meteorological season ("winter", "spring", "summer", "autumn")
#' for a given date and latitude. Hemisphere is inferred from the sign of
#' latitude (positive = Northern Hemisphere, negative = Southern Hemisphere).
#'
#' @param date A `Date` or `POSIXct` object, or a character string coercible
#'   to `Date` (e.g., `"2024-03-15"`).
#' @param lat Numeric. Latitude in decimal degrees. Positive = N hemisphere.
#'
#' @return A character string: one of `"winter"`, `"spring"`, `"summer"`,
#'   `"autumn"`.
#'
#' @export
#' @examples
#' detect_season("2024-01-15", lat = 51.5)   # "winter" (UK)
#' detect_season("2024-07-15", lat = 51.5)   # "summer"
#' detect_season("2024-01-15", lat = -33.9)  # "summer" (Sydney)
detect_season <- function(date, lat) {
  if (is.null(date) || is.null(lat) || is.na(lat) || is.na(date)) {
    return(NA_character_)
  }
  date <- as.Date(date)
  if (is.na(date)) return(NA_character_)
  month <- as.integer(format(date, "%m"))

  # Meteorological seasons (by month)
  nh_season <- dplyr::case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3, 4, 5)  ~ "spring",
    month %in% c(6, 7, 8)  ~ "summer",
    month %in% c(9, 10, 11) ~ "autumn"
  )

  # Flip for Southern Hemisphere
  if (lat < 0) {
    nh_season <- dplyr::recode(nh_season,
      winter = "summer",
      summer = "winter",
      spring = "autumn",
      autumn = "spring"
    )
  }

  nh_season
}


#' Detect season for each row in a dataframe
#'
#' @description
#' Vectorised wrapper around [detect_season()] for use on dataframes.
#'
#' @param df A dataframe containing at minimum a date column and a latitude column.
#' @param date_col Name of the date column (default `"date"`).
#' @param lat_col  Name of the latitude column (default `"lat"`).
#'
#' @return The input dataframe with an additional column `season`.
#' @export
add_season_column <- function(df, date_col = NULL, lat_col = "lat") {
  if (is.null(date_col)) {
    date_col <- intersect(c("date", "datetime", "timestamp", "Date", "DateTime"), names(df))[1]
    if (is.na(date_col))
      cli::cli_abort("No date/datetime column found. Supply a date column name via {.arg date_col}.")
  }
  if (!date_col %in% names(df)) {
    cli::cli_abort("Column {.val {date_col}} not found. Supply a date column or set {.arg date_col}.")
  }
  if (!lat_col %in% names(df)) {
    cli::cli_abort("Column {.val {lat_col}} not found. Supply a latitude column or set {.arg lat_col}.")
  }
  df$season <- mapply(detect_season,
                      date = df[[date_col]],
                      lat  = df[[lat_col]])
  df
}
