# =============================================================================
# Live environmental data integration framework
# =============================================================================
#
# Architecture:
#  - All live data fetching is OFF by default. Functions work entirely from
#    manually supplied data when fetch_live = FALSE (the default).
#  - API credentials are stored in R options(), never in package state.
#  - Each downstream function (score_predation_risk, score_hab_risk, etc.)
#    accepts both manual data input AND fetch_live = TRUE as alternatives.
#  - Graceful degradation: if live fetch fails, a warning is issued and the
#    function proceeds without the live layer rather than aborting.
#
# Supported live data sources:
#  - CMEMS (Copernicus Marine Service): SST, salinity, chlorophyll
#  - ICES HAB database: harmful algal bloom event records
#  - ICES VMS / swept-area ratio: trawling intensity
#  - EMODnet Biology: predator occurrence data
#  - FSA / DAERA shellfish classification: EC 854/2004 water class areas
#
# Setup:
#  oystermapR_live_config(cmems_user = "x", cmems_password = "y", ...)
#  fetch_live_environmental_data(survey, sources = "all")

# \u2500\u2500 Configuration \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Configure live data API credentials for oystermapR
#'
#' @description
#' Stores API credentials in R session options so live data fetching can
#' authenticate with external services. Credentials are **not** written to
#' disk and are lost when the R session ends.
#'
#' All parameters are optional \u2014 only set credentials for the services you
#' intend to use. Functions that require a missing credential will warn
#' and fall back to manual data rather than aborting.
#'
#' @section Free registrations:
#' - **CMEMS**: Register at [marine.copernicus.eu](https://marine.copernicus.eu)
#' - **ICES**: API access is open (no key required for HAB/VMS endpoints)
#' - **EMODnet Biology**: Open WFS, no key required
#' - **FSA/DAERA**: Open REST API, no key required
#'
#' @param cmems_user Character. CMEMS username.
#' @param cmems_password Character. CMEMS password.
#' @param ices_key Character. ICES API key (currently optional; ICES APIs are
#'   largely open).
#' @param show_config Logical. Print current configuration (credentials masked).
#'   Default FALSE.
#'
#' @return Invisibly returns a named list of the options set.
#' @export
#' @examples
#' \dontrun{
#' oystermapR_live_config(
#'   cmems_user     = "myusername",
#'   cmems_password = "mypassword"
#' )
#' # Then use fetch_live = TRUE in any supporting function
#' score_hab_risk(result, species = "ostrea_edulis", fetch_live = TRUE)
#' }
oystermapR_live_config <- function(cmems_user     = NULL,
                                    cmems_password  = NULL,
                                    ices_key        = NULL,
                                    show_config     = FALSE) {
  if (!is.null(cmems_user))     options(oystermapR.cmems_user     = cmems_user)
  if (!is.null(cmems_password)) options(oystermapR.cmems_password = cmems_password)
  if (!is.null(ices_key))       options(oystermapR.ices_key       = ices_key)

  if (show_config) {
    cli::cli_h2("oystermapR live data configuration")
    mask <- function(x) if (is.null(x)) "<not set>" else paste0(substr(x, 1, 2), "****")
    cli::cli_inform(c(
      "CMEMS user"     = mask(getOption("oystermapR.cmems_user")),
      "CMEMS password" = mask(getOption("oystermapR.cmems_password")),
      "ICES key"       = mask(getOption("oystermapR.ices_key")),
      "EMODnet"        = "open (no key required)",
      "FSA/DAERA"      = "open (no key required)"
    ))
  }

  invisible(list(
    cmems_user     = getOption("oystermapR.cmems_user"),
    cmems_password = getOption("oystermapR.cmems_password"),
    ices_key       = getOption("oystermapR.ices_key")
  ))
}


# \u2500\u2500 Master fetch function \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Fetch live environmental data for a survey extent
#'
#' @description
#' Convenience wrapper that fetches data from one or more live sources and
#' returns a named list of dataframes that can be passed to the relevant
#' scoring functions. Each source can be fetched independently or all at once.
#'
#' This function requires internet access and registered API credentials where
#' applicable (see [oystermapR_live_config()]). If a source fails, it is
#' silently skipped and not included in the output \u2014 the function never aborts
#' due to a single failed source.
#'
#' @param survey Dataframe with `lat` and `lon` columns defining the survey
#'   extent. The bounding box of these coordinates is used as the query extent.
#' @param sources Character vector. Which data sources to fetch. Options:
#'   `"cmems"`, `"ices_hab"`, `"ices_vms"`, `"emodnet_predators"`,
#'   `"fsa_shellfish"`. Use `"all"` to attempt all sources.
#' @param date_range Character vector of length 2: `c("YYYY-MM-DD", "YYYY-MM-DD")`.
#'   Date range for time-windowed queries (CMEMS, ICES HAB). Defaults to
#'   the last 5 years.
#' @param verbose Logical. Print progress (default TRUE).
#'
#' @return Named list. Each successfully fetched source produces one element:
#'   - `$cmems`: SST, salinity, chlorophyll grid
#'   - `$ices_hab`: HAB event records within extent
#'   - `$ices_vms`: Swept-area ratio per ICES c-square
#'   - `$emodnet_predators`: Predator occurrence records
#'   - `$fsa_shellfish`: Shellfish classification polygons / area descriptions
#'
#' @export
#' @examples
#' \dontrun{
#' oystermapR_live_config(cmems_user = "u", cmems_password = "p")
#' live <- fetch_live_environmental_data(survey, sources = c("ices_hab", "ices_vms"))
#' result <- score_hab_risk(result, hab_data = live$ices_hab, species = "ostrea_edulis")
#' result <- score_anthropogenic_disturbance(result, trawling_data = live$ices_vms)
#' }
fetch_live_environmental_data <- function(survey,
                                           sources    = "all",
                                           date_range = NULL,
                                           verbose    = TRUE) {

  if (!all(c("lat","lon") %in% names(survey)))
    cli::cli_abort("survey must contain 'lat' and 'lon' columns.")

  if (identical(sources, "all"))
    sources <- c("cmems","ices_hab","ices_vms","emodnet_predators","fsa_shellfish")

  if (is.null(date_range)) {
    end_date   <- Sys.Date()
    start_date <- end_date - 365 * 5
    date_range <- as.character(c(start_date, end_date))
  }

  bbox <- c(
    lon_min = min(survey$lon, na.rm = TRUE),
    lon_max = max(survey$lon, na.rm = TRUE),
    lat_min = min(survey$lat, na.rm = TRUE),
    lat_max = max(survey$lat, na.rm = TRUE)
  )

  if (verbose)
    cli::cli_h2("Fetching live environmental data \u2014 {length(sources)} source{?s}")

  out <- list()

  for (src in sources) {
    result <- tryCatch({
      switch(src,
        cmems              = .fetch_cmems(bbox, date_range, verbose),
        ices_hab           = .fetch_ices_hab(bbox, date_range, verbose),
        ices_vms           = .fetch_ices_vms(bbox, verbose),
        emodnet_predators  = .fetch_emodnet_predators(bbox, verbose),
        fsa_shellfish      = .fetch_fsa_shellfish(bbox, verbose),
        { cli::cli_warn("Unknown source: {src}"); NULL }
      )
    }, error = function(e) {
      cli::cli_warn("Live fetch failed for '{src}': {conditionMessage(e)}")
      NULL
    })
    if (!is.null(result)) out[[src]] <- result
  }

  if (verbose) {
    fetched <- names(out)
    failed  <- setdiff(sources, fetched)
    cli::cli_inform(c(
      "\u2713" = "Fetched: {paste(fetched, collapse=', ')}",
      if (length(failed) > 0) c("!" = "Not available: {paste(failed, collapse=', ')}")
    ))
  }

  out
}


# \u2500\u2500 Internal fetch helpers \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

.check_httr <- function() {
  if (!requireNamespace("httr", quietly = TRUE))
    cli::cli_abort(c(
      "Live data fetching requires the {.pkg httr} package.",
      "i" = "Install with: {.code install.packages('httr')}"
    ))
}

.fetch_cmems <- function(bbox, date_range, verbose) {
  .check_httr()
  user <- getOption("oystermapR.cmems_user")
  pass <- getOption("oystermapR.cmems_password")

  if (is.null(user) || is.null(pass))
    cli::cli_abort(c(
      "CMEMS credentials not set.",
      "i" = "Run {.code oystermapR_live_config(cmems_user='...', cmems_password='...')} first."
    ))

  if (verbose) cli::cli_inform("  Fetching CMEMS NW shelf SST/salinity/chlorophyll...")

  # CMEMS NWSHELF Physics Analysis product (NWS_PHY_ANFC)
  # Returns a simplified grid summary \u2014 full NetCDF download requires
  # the CMEMS Python toolbox (copernicusmarine); here we use the
  # WMTS/subset REST endpoint for a lightweight lat/lon/value table.
  url <- paste0(
    "https://nrt.cmems-du.eu/motu-web/Motu?action=productdownload",
    "&service=NORTHWESTSHELF_ANALYSIS_FORECAST_PHY_004_013-TDS",
    "&product=cmems_mod_nws_phy-sst_anfc_0.03deg_P1D-m",
    "&x_lo=", bbox["lon_min"], "&x_hi=", bbox["lon_max"],
    "&y_lo=", bbox["lat_min"], "&y_hi=", bbox["lat_max"],
    "&t_lo=", date_range[1], "&t_hi=", date_range[2],
    "&variable=thetao&out_fmt=ascii"
  )

  resp <- httr::GET(url, httr::authenticate(user, pass), httr::timeout(60))

  if (httr::http_error(resp))
    cli::cli_abort("CMEMS request failed: HTTP {httr::status_code(resp)}")

  content_txt <- httr::content(resp, "text", encoding = "UTF-8")

  # Parse CSV-like ASCII response
  lines <- strsplit(content_txt, "\n")[[1]]
  lines <- lines[!grepl("^#|^$", lines)]
  if (length(lines) < 2) return(NULL)

  df <- utils::read.csv(text = paste(lines, collapse = "\n"), stringsAsFactors = FALSE)
  if (verbose)
    cli::cli_inform("    CMEMS: {nrow(df)} grid point{?s} retrieved.")
  df
}

.fetch_ices_hab <- function(bbox, date_range, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching ICES HAB event records...")

  # ICES HAB database web service
  url <- paste0(
    "https://webservices.ices.dk/HAB/HABindex/HabListbyArea?",
    "habname=all",
    "&latmin=", bbox["lat_min"], "&latmax=", bbox["lat_max"],
    "&lonmin=", bbox["lon_min"], "&lonmax=", bbox["lon_max"],
    "&startdate=", date_range[1], "&enddate=", date_range[2]
  )

  resp <- httr::GET(url, httr::timeout(30))
  if (httr::http_error(resp))
    cli::cli_abort("ICES HAB request failed: HTTP {httr::status_code(resp)}")

  content_txt <- httr::content(resp, "text", encoding = "UTF-8")

  # ICES HAB returns JSON or CSV depending on endpoint version
  tryCatch({
    df <- jsonlite::fromJSON(content_txt, flatten = TRUE)
    if (is.data.frame(df) && nrow(df) > 0) {
      if (verbose)
        cli::cli_inform("    ICES HAB: {nrow(df)} event record{?s} retrieved.")
      return(df)
    }
    NULL
  }, error = function(e) {
    # Fallback: try CSV parse
    lines <- strsplit(content_txt, "\n")[[1]]
    lines <- lines[!grepl("^#|^$", lines)]
    if (length(lines) < 2) return(NULL)
    utils::read.csv(text = paste(lines, collapse = "\n"), stringsAsFactors = FALSE)
  })
}

.fetch_ices_vms <- function(bbox, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching ICES VMS swept-area ratio...")

  # ICES swept-area ratio data (publicly available aggregated VMS)
  # Data available via ICES Data Portal as pre-aggregated c-square datasets
  # We query the most recent available year's summary
  url <- paste0(
    "https://gis.ices.dk/geoserver/ices/ows?service=WFS&version=1.0.0",
    "&request=GetFeature&typeName=ices:SweptAreaRatio",
    "&outputFormat=json",
    "&CQL_FILTER=BBOX(geom,",
    bbox["lon_min"], ",", bbox["lat_min"], ",",
    bbox["lon_max"], ",", bbox["lat_max"], ")"
  )

  resp <- httr::GET(url, httr::timeout(45))
  if (httr::http_error(resp))
    cli::cli_abort("ICES VMS request failed: HTTP {httr::status_code(resp)}")

  content_txt <- httr::content(resp, "text", encoding = "UTF-8")

  tryCatch({
    geo  <- jsonlite::fromJSON(content_txt, flatten = TRUE)
    feats <- geo$features
    if (!is.null(feats) && nrow(feats) > 0) {
      df <- feats$properties
      # Extract centroid coordinates from geometry
      coords <- do.call(rbind, lapply(feats$geometry$coordinates, function(x) {
        c(lon = mean(x[,1]), lat = mean(x[,2]))
      }))
      df$lon <- coords[,"lon"]
      df$lat <- coords[,"lat"]
      if (verbose)
        cli::cli_inform("    ICES VMS: {nrow(df)} c-square{?s} retrieved.")
      return(df)
    }
    NULL
  }, error = function(e) NULL)
}

.fetch_emodnet_predators <- function(bbox, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching EMODnet predator occurrences...")

  # EMODnet Biology WFS \u2014 Asterias rubens (starfish), Carcinus maenas (green crab)
  # AphiaIDs: Asterias rubens = 123776, Carcinus maenas = 107381
  species_ids <- c("123776", "107381")  # Asterias rubens, Carcinus maenas

  fetch_species <- function(aphia_id) {
    url <- paste0(
      "https://geo.vliz.be/geoserver/Emodnetbio/wfs?",
      "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature",
      "&TypeName=Emodnetbio:mediseh_oyster_f",
      # Using occurrence endpoint:
      "&outputFormat=application/json",
      "&CQL_FILTER=aphiaid=", aphia_id,
      "+AND+BBOX(the_geom,",
      bbox["lon_min"], ",", bbox["lat_min"], ",",
      bbox["lon_max"], ",", bbox["lat_max"], ")"
    )
    resp <- httr::GET(url, httr::timeout(30))
    if (httr::http_error(resp)) return(NULL)
    tryCatch({
      geo <- jsonlite::fromJSON(httr::content(resp, "text", encoding="UTF-8"), flatten=TRUE)
      feats <- geo$features
      if (!is.null(feats) && nrow(feats) > 0) {
        df <- feats$properties
        coords <- do.call(rbind, lapply(feats$geometry$coordinates, function(x) x))
        df$lon <- coords[,1]; df$lat <- coords[,2]
        df$aphia_id <- aphia_id
        df
      } else NULL
    }, error = function(e) NULL)
  }

  all_df <- do.call(rbind, lapply(species_ids, fetch_species))
  if (!is.null(all_df) && nrow(all_df) > 0) {
    if (verbose)
      cli::cli_inform("    EMODnet: {nrow(all_df)} predator occurrence{?s} retrieved.")
    return(all_df)
  }
  NULL
}

.fetch_fsa_shellfish <- function(bbox, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching FSA/DAERA shellfish classification areas...")

  # FSA England/Wales Open Data API for classified shellfish harvesting areas
  # This returns polygon features with their classification (A/B/C/Prohibited)
  url <- paste0(
    "https://environment.data.gov.uk/arcgis/rest/services/EA/",
    "ShellFishHarvestingAreas/MapServer/0/query?",
    "geometry=", bbox["lon_min"], ",", bbox["lat_min"], ",",
    bbox["lon_max"], ",", bbox["lat_max"],
    "&geometryType=esriGeometryEnvelope",
    "&spatialRel=esriSpatialRelIntersects",
    "&outFields=*&f=json"
  )

  resp <- httr::GET(url, httr::timeout(30))
  if (httr::http_error(resp))
    cli::cli_abort("FSA shellfish classification request failed: HTTP {httr::status_code(resp)}")

  content_txt <- httr::content(resp, "text", encoding = "UTF-8")

  tryCatch({
    geo   <- jsonlite::fromJSON(content_txt, flatten = TRUE)
    feats <- geo$features
    if (!is.null(feats) && nrow(feats) > 0) {
      df <- feats$attributes
      if (verbose)
        cli::cli_inform("    FSA: {nrow(df)} classified area{?s} retrieved.")
      return(df)
    }
    NULL
  }, error = function(e) NULL)
}
