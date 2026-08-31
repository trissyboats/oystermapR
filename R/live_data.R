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
#' All parameters are optional - only set credentials for the services you
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
#' silently skipped and not included in the output - the function never aborts
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

# -- CMEMS detection helpers --------------------------------------------------

# Common locations where pip/pipx/conda/brew install copernicusmarine on macOS/Linux.
# RStudio on macOS strips PATH on launch, so Sys.which() often misses these.
.cm_search_paths <- function() {
  py_versions <- paste0("3.", 8:13)
  c(
    # Direct copernicusmarine executable
    "/usr/local/bin/copernicusmarine",
    "/opt/homebrew/bin/copernicusmarine",
    path.expand("~/.local/bin/copernicusmarine"),
    unlist(lapply(py_versions, function(v)
      path.expand(paste0("~/Library/Python/", v, "/bin/copernicusmarine")))),
    path.expand("~/opt/anaconda3/bin/copernicusmarine"),
    path.expand("~/anaconda3/bin/copernicusmarine"),
    path.expand("~/miniconda3/bin/copernicusmarine"),
    path.expand("~/opt/miniconda3/bin/copernicusmarine"),
    # pipx installs here
    path.expand("~/.local/pipx/venvs/copernicusmarine/bin/copernicusmarine"),
    # Windows (for cross-platform use)
    path.expand("~/AppData/Roaming/Python/Scripts/copernicusmarine.exe")
  )
}

# Returns TRUE if copernicusmarine is callable in any of the standard ways.
.cmems_python_available <- function() {
  # Method 1: direct executable on PATH
  if (nchar(Sys.which("copernicusmarine")) > 0) return(TRUE)
  # Method 2: common macOS/Linux install locations (RStudio strips PATH)
  for (loc in .cm_search_paths()) {
    if (nchar(loc) > 0 && file.exists(loc)) return(TRUE)
  }
  # Method 3: importable via a Python on PATH
  for (py in c("python3", "python")) {
    path <- Sys.which(py)
    if (nchar(path) == 0) next
    ok <- suppressWarnings(
      system2(path, c("-c", "import copernicusmarine"),
              stdout = FALSE, stderr = FALSE) == 0L
    )
    if (ok) return(TRUE)
  }
  FALSE
}

# Returns list(exe, args_prefix) for the best available copernicusmarine invocation.
.copernicusmarine_cmd <- function() {
  # Direct command on PATH
  cm <- Sys.which("copernicusmarine")
  if (nchar(cm) > 0) return(list(exe = cm, prefix = character(0)))
  # Common install locations
  for (loc in .cm_search_paths()) {
    if (nchar(loc) > 0 && file.exists(loc))
      return(list(exe = loc, prefix = character(0)))
  }
  # Python -m fallback
  for (py in c("python3", "python")) {
    path <- Sys.which(py)
    if (nchar(path) == 0) next
    ok <- suppressWarnings(
      system2(path, c("-c", "import copernicusmarine"),
              stdout = FALSE, stderr = FALSE) == 0L
    )
    if (ok) return(list(exe = path, prefix = c("-m", "copernicusmarine")))
  }
  NULL
}

# -- CMEMS via copernicusmarine Python CLI ------------------------------------
# Requires: pip install copernicusmarine
# Products fetched (NW European shelf, daily means):
#   cmems_mod_nws_phy-sst_anfc_0.03deg_P1D-m  -> thetao (SST, degC)
#   cmems_mod_nws_phy-sal_anfc_0.03deg_P1D-m  -> so     (salinity, PSU)
#   cmems_mod_nws_bgc-chl_my_7km-3D_P1D-m     -> chl    (chlorophyll, mg/m3)

.fetch_cmems_python <- function(bbox, date_range, user, pass, verbose) {
  cmd <- .copernicusmarine_cmd()
  if (is.null(cmd))
    cli::cli_abort("copernicusmarine not found \u2014 install with: pip install copernicusmarine")
  if (verbose)
    cli::cli_inform("  CMEMS: using copernicusmarine CLI ({cmd$exe})...")

  # copernicusmarine v2.x removed --username/--password CLI flags.
  # Credentials are passed via environment variables instead.
  old_u <- Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME")
  old_p <- Sys.getenv("COPERNICUSMARINE_SERVICE_PASSWORD")
  Sys.setenv(
    COPERNICUSMARINE_SERVICE_USERNAME = user,
    COPERNICUSMARINE_SERVICE_PASSWORD = pass
  )
  on.exit({
    if (nchar(old_u) > 0) Sys.setenv(COPERNICUSMARINE_SERVICE_USERNAME = old_u)
    else Sys.unsetenv("COPERNICUSMARINE_SERVICE_USERNAME")
    if (nchar(old_p) > 0) Sys.setenv(COPERNICUSMARINE_SERVICE_PASSWORD = old_p)
    else Sys.unsetenv("COPERNICUSMARINE_SERVICE_PASSWORD")
  }, add = TRUE)

  # v2.x outputs NetCDF by default (CSV output was dropped).
  # terra is in Imports so it is always available to parse the .nc files.
  #
  # Each variable lists candidate dataset IDs in preference order:
  #   1. NWS regional (0.03 deg, highest res for European shelf)
  #   2. Global ANFC  (0.083 deg, always available, covers any bbox)
  # Use _anfc_ (analysis-forecast) products so queries work up to today.
  # The _my_ (multi-year reanalysis) products lag ~3 months behind.
  products <- list(
    list(
      col = "temperature", var = "thetao",
      ids = c(
        "cmems_mod_nws_phy-sst_anfc_0.03deg_P1D-m",  # NWS preferred
        "cmems_mod_nws_phy_anfc_0.03deg_P1D-m",       # alt NWS naming
        "cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m" # global fallback
      )
    ),
    list(
      col = "salinity", var = "so",
      ids = c(
        "cmems_mod_nws_phy-sal_anfc_0.03deg_P1D-m",
        "cmems_mod_nws_phy_anfc_0.03deg_P1D-m",
        "cmems_mod_glo_phy-so_anfc_0.083deg_P1D-m"
      )
    ),
    list(
      col = "chlorophyll_a", var = "chl",
      ids = c(
        "cmems_mod_nws_bgc-chl_anfc_7km-3D_P1D-m",   # NWS ANFC (not _my_)
        "cmems_mod_nws_bgc_anfc_7km-3D_P1D-m",
        "cmems_mod_glo_bgc-chl_anfc_0.25deg_P1D-m"
      )
    )
  )

  frames  <- list()
  tmp_dir <- tempdir()

  for (p in products) {
    out_file <- file.path(tmp_dir, paste0("cmems_", p$col, ".nc"))
    if (file.exists(out_file)) unlink(out_file)

    # Try each candidate dataset ID until one succeeds
    succeeded <- FALSE
    for (dataset_id in p$ids) {
      args <- c(
        cmd$prefix,
        "subset",
        "--dataset-id",          dataset_id,
        "--variable",            p$var,
        "--minimum-longitude",   as.character(bbox["lon_min"]),
        "--maximum-longitude",   as.character(bbox["lon_max"]),
        "--minimum-latitude",    as.character(bbox["lat_min"]),
        "--maximum-latitude",    as.character(bbox["lat_max"]),
        "--start-datetime",      paste0(date_range[1], "T00:00:00"),
        "--end-datetime",        paste0(date_range[2], "T00:00:00"),
        "--minimum-depth",       "0.0",
        "--maximum-depth",       "10.0",
        "--output-directory",    tmp_dir,
        "--output-filename",     basename(out_file),
        "--log-level",           "WARN"
      )

      stderr_file <- tempfile()
      ret <- suppressWarnings(
        system2(cmd$exe, args, stdout = FALSE, stderr = stderr_file)
      )

      if (ret == 0L && file.exists(out_file)) {
        if (verbose)
          cli::cli_inform("    {p$col}: using dataset {dataset_id}")
        unlink(stderr_file)
        succeeded <- TRUE
        break
      }
      unlink(stderr_file)
    }

    if (!succeeded) {
      cli::cli_warn("copernicusmarine: no working dataset found for {p$col} (tried: {paste(p$ids, collapse=', ')})")
      next
    }

    # Parse the NetCDF.
    # CMEMS stores coordinates in dimension variables (latitude, longitude) that
    # GDAL can't always auto-detect, causing terra to fall back to pixel indices.
    # We use ncdf4 directly when available \u2014 it reads dimension variables reliably.
    df_parsed <- if (requireNamespace("ncdf4", quietly = TRUE)) {
      tryCatch({
        nc  <- ncdf4::nc_open(out_file)
        on.exit(ncdf4::nc_close(nc), add = TRUE)

        # Locate lat/lon dimension variables
        dim_nms <- names(nc$dim)
        lat_nm  <- intersect(dim_nms, c("latitude",  "lat", "y"))[1]
        lon_nm  <- intersect(dim_nms, c("longitude", "lon", "x"))[1]
        if (is.na(lat_nm) || is.na(lon_nm)) stop("lat/lon dims not found")

        lats <- as.vector(ncdf4::ncvar_get(nc, lat_nm))
        lons <- as.vector(ncdf4::ncvar_get(nc, lon_nm))

        # Read the variable; find which array dimensions are lat and lon.
        # collapse_degen = FALSE keeps singleton dims (e.g. lon=1, depth=1)
        # so the array always has the full shape and apply() works correctly.
        vals         <- ncdf4::ncvar_get(nc, p$var, collapse_degen = FALSE)
        var_dim_nms  <- sapply(nc$var[[p$var]]$dim, function(d) d$name)
        lon_pos      <- which(var_dim_nms == lon_nm)
        lat_pos      <- which(var_dim_nms == lat_nm)
        spatial_pos  <- c(lon_pos, lat_pos)

        # Average over all non-spatial dimensions (time, depth, etc.)
        vals_2d <- if (length(dim(vals)) > 2)
          apply(vals, spatial_pos, mean, na.rm = TRUE)
        else
          vals

        # Build long-format dataframe
        if (lon_pos < lat_pos) {
          df_out <- expand.grid(lon = lons, lat = lats)
        } else {
          df_out <- expand.grid(lat = lats, lon = lons)
        }
        df_out[[p$col]] <- as.vector(vals_2d)
        df_out[!is.na(df_out[[p$col]]), , drop = FALSE]
      }, error = function(e) {
        cli::cli_warn("ncdf4 parse failed for {p$col}: {conditionMessage(e)}")
        NULL
      })
    } else {
      # Fallback to terra \u2014 coordinates may be pixel-index if GDAL can't read them
      tryCatch({
        r      <- suppressWarnings(terra::rast(out_file))
        r_mean <- if (terra::nlyr(r) > 1) terra::mean(r, na.rm = TRUE) else r
        xy     <- as.data.frame(r_mean, xy = TRUE)
        names(xy)[1:3] <- c("lon", "lat", p$col)
        xy[!is.na(xy[[p$col]]), , drop = FALSE]
      }, error = function(e) {
        cli::cli_warn("terra parse failed for {p$col}: {conditionMessage(e)}")
        NULL
      })
    }

    if (!is.null(df_parsed) && nrow(df_parsed) > 0) frames[[p$col]] <- df_parsed
    unlink(out_file)
  }

  if (length(frames) == 0) return(NULL)

  # Merge all products. Different CMEMS grids (0.083 deg vs 7 km) don't align
  # exactly, so we round coordinates to 2 decimal places (~1 km precision)
  # before joining \u2014 close enough for survey-scale environmental augmentation.
  rounded <- lapply(names(frames), function(nm) {
    df        <- frames[[nm]]
    df$lat_r  <- round(df$lat, 2)
    df$lon_r  <- round(df$lon, 2)
    df[c("lat_r", "lon_r", nm)]
  })

  out <- Reduce(function(a, b) merge(a, b, by = c("lat_r", "lon_r"), all = TRUE),
                rounded)
  names(out)[1:2] <- c("lat", "lon")
  # Drop rows where every variable is NA
  val_cols <- names(frames)
  out <- out[rowSums(!is.na(out[val_cols])) > 0, , drop = FALSE]

  if (verbose)
    cli::cli_inform("    CMEMS (Python): {nrow(out)} grid point{?s}, columns: {paste(names(out), collapse=', ')}")
  out
}

# -- CMEMS via ERDDAP REST (no Python needed) ---------------------------------
# Uses the official Copernicus Marine ERDDAP server.
# Still needs CMEMS credentials (basic auth).
# Products:
#   cmems_mod_nws_phy-sst_anfc_0.03deg_P1D-m  -> thetao
#   cmems_mod_nws_phy-sal_anfc_0.03deg_P1D-m  -> so
#   cmems_mod_nws_bgc-chl_my_7km-3D_P1D-m     -> chl

.fetch_cmems_erddap <- function(bbox, date_range, user, pass, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  CMEMS: using ERDDAP REST endpoint...")

  # CMEMS ERDDAP \u2014 served through the Copernicus Marine data portal.
  # The nrt (near-real-time) subdomain hosts the same products as the main portal.
  base <- "https://nrt.cmems-du.eu/thredds/dodsC"
  # Use the REST/CSV subset endpoint instead of OPeNDAP for simpler parsing
  erddap_base <- "https://nrt.cmems-du.eu/erddap/griddap"

  # Time range as ISO strings with a midday anchor to avoid day-boundary issues
  t0 <- paste0(date_range[1], "T12:00:00Z")
  t1 <- paste0(date_range[2], "T12:00:00Z")

  products <- list(
    list(
      id  = "cmems_mod_nws_phy-sst_anfc_0.03deg_P1D-m",
      var = "thetao",
      col = "temperature",
      depth_dim = TRUE
    ),
    list(
      id  = "cmems_mod_nws_phy-sal_anfc_0.03deg_P1D-m",
      var = "so",
      col = "salinity",
      depth_dim = TRUE
    ),
    list(
      id  = "cmems_mod_nws_bgc-chl_my_7km-3D_P1D-m",
      var = "chl",
      col = "chlorophyll_a",
      depth_dim = TRUE
    )
  )

  frames <- list()

  for (p in products) {
    # ERDDAP griddap CSV URL
    # Dimension order: [time][depth][latitude][longitude]
    depth_slice <- if (isTRUE(p$depth_dim)) "[(0.0):1:(0.0)]" else ""
    url <- paste0(
      erddap_base, "/", p$id, ".csv?",
      p$var,
      "[(", t0, "):1:(", t1, ")]",
      depth_slice,
      "[(", bbox["lat_min"], "):1:(", bbox["lat_max"], ")]",
      "[(", bbox["lon_min"], "):1:(", bbox["lon_max"], ")]"
    )

    resp <- tryCatch(
      httr::GET(url, httr::authenticate(user, pass), httr::timeout(60)),
      error = function(e) {
        cli::cli_warn("ERDDAP request error for {p$col}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(resp) || httr::http_error(resp)) {
      cli::cli_warn("ERDDAP HTTP {httr::status_code(resp)} for {p$col} \u2014 skipping")
      next
    }

    txt <- httr::content(resp, "text", encoding = "UTF-8")
    lines <- strsplit(txt, "\n")[[1]]
    # ERDDAP CSV has a units row on line 2 \u2014 drop it, keep header + data
    if (length(lines) < 3) next
    lines <- c(lines[1], lines[3:length(lines)])
    lines <- lines[nchar(trimws(lines)) > 0]
    df <- tryCatch(
      utils::read.csv(text = paste(lines, collapse = "\n"), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) next

    # Rename value column and keep lat/lon
    lon_col <- grep("longitude|lon",  names(df), ignore.case = TRUE, value = TRUE)[1]
    lat_col <- grep("latitude|lat",   names(df), ignore.case = TRUE, value = TRUE)[1]
    val_col <- grep(paste0("^", p$var), names(df), value = TRUE)[1]

    if (!is.na(lon_col) && !is.na(lat_col) && !is.na(val_col)) {
      frames[[p$col]] <- data.frame(
        lon              = df[[lon_col]],
        lat              = df[[lat_col]],
        setNames(list(df[[val_col]]), p$col),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(frames) == 0) return(NULL)

  out <- Reduce(function(a, b) merge(a, b, by = c("lat","lon"), all = TRUE), frames)
  if (verbose)
    cli::cli_inform("    CMEMS (ERDDAP): {nrow(out)} grid point{?s}, columns: {paste(names(out), collapse=', ')}")
  out
}

# -- CMEMS dispatcher ---------------------------------------------------------
# Uses Python copernicusmarine if available; falls back to ERDDAP otherwise.

.fetch_cmems <- function(bbox, date_range, verbose) {
  user <- getOption("oystermapR.cmems_user")
  pass <- getOption("oystermapR.cmems_password")

  if (is.null(user) || is.null(pass))
    cli::cli_abort(c(
      "CMEMS credentials not set.",
      "i" = "Run {.code oystermapR_live_config(cmems_user='...', cmems_password='...')} first.",
      "i" = "Register free at {.url https://marine.copernicus.eu}"
    ))

  if (.cmems_python_available()) {
    .fetch_cmems_python(bbox, date_range, user, pass, verbose)
  } else {
    if (verbose)
      cli::cli_inform("  Python/copernicusmarine not found \u2014 falling back to ERDDAP.")
    .check_httr()
    .fetch_cmems_erddap(bbox, date_range, user, pass, verbose)
  }
}

.fetch_ices_hab <- function(bbox, date_range, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching ICES HAB event records...")

  # ICES HAB database \u2014 try multiple known endpoints in order.
  # The webservices.ices.dk domain was decommissioned; the GeoServer at
  # gis.ices.dk now hosts spatial queries. The HAB point layer is
  # "ices_hab:HABEvents" (WFS).
  urls <- list(
    # Option 1: ICES GeoServer WFS \u2014 HAB event points
    list(
      url = paste0(
        "https://gis.ices.dk/geoserver/ices_hab/ows?service=WFS&version=2.0.0",
        "&request=GetFeature&typeNames=ices_hab:HABEvents",
        "&outputFormat=application%2Fjson",
        "&count=1000",
        "&CQL_FILTER=BBOX(geom,",
        bbox["lon_min"], ",", bbox["lat_min"], ",",
        bbox["lon_max"], ",", bbox["lat_max"], ")",
        "%20AND%20EventDate%20BETWEEN%20'", date_range[1],
        "'%20AND%20'", date_range[2], "'"
      ),
      parse = "geojson"
    ),
    # Option 2: ICES GeoServer \u2014 alternative layer name
    list(
      url = paste0(
        "https://gis.ices.dk/geoserver/ices/ows?service=WFS&version=1.0.0",
        "&request=GetFeature&typeName=ices:HABEvents",
        "&outputFormat=json",
        "&CQL_FILTER=BBOX(geom,",
        bbox["lon_min"], ",", bbox["lat_min"], ",",
        bbox["lon_max"], ",", bbox["lat_max"], ")"
      ),
      parse = "geojson"
    ),
    # Option 3: ICES DATRAS-style REST (some HAB products here)
    list(
      url = paste0(
        "https://datras.ices.dk/WebServices/DATRASWebService.asmx/",
        "getHABEventsByArea?",
        "latMin=", bbox["lat_min"], "&latMax=", bbox["lat_max"],
        "&lonMin=", bbox["lon_min"], "&lonMax=", bbox["lon_max"],
        "&startYear=", format(as.Date(date_range[1]), "%Y"),
        "&endYear=",   format(as.Date(date_range[2]), "%Y")
      ),
      parse = "json"
    )
  )

  for (u in urls) {
    resp <- tryCatch(
      httr::GET(u$url, httr::timeout(30),
                httr::add_headers(Accept = "application/json")),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::http_error(resp)) next

    txt <- httr::content(resp, "text", encoding = "UTF-8")
    df  <- tryCatch({
      parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
      if (u$parse == "geojson" && !is.null(parsed$features)) {
        feats <- parsed$features
        if (nrow(feats) > 0) feats$properties else NULL
      } else if (is.data.frame(parsed) && nrow(parsed) > 0) {
        parsed
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(df) && nrow(df) > 0) {
      if (verbose)
        cli::cli_inform("    ICES HAB: {nrow(df)} event record{?s} retrieved.")
      return(df)
    }
  }

  # No HAB records found (normal for many areas \u2014 not an error)
  if (verbose) cli::cli_inform("    ICES HAB: no records in this area/period.")
  NULL
}

.fetch_ices_vms <- function(bbox, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching ICES VMS swept-area ratio...")

  # ICES swept-area ratio \u2014 aggregated VMS data via ICES GeoServer.
  # Layer name updated to current ICES GeoServer catalogue (2024).
  # Primary layer: ices_vms:SweptAreaRatio_2022 (most recent published year).
  # Falls back to the standard ices:SweptAreaRatio if the year-specific layer 404s.
  urls <- c(
    paste0(
      "https://gis.ices.dk/geoserver/ices_vms/ows?service=WFS&version=2.0.0",
      "&request=GetFeature&typeNames=ices_vms:SweptAreaRatio_2022",
      "&outputFormat=application%2Fjson",
      "&count=500",
      "&CQL_FILTER=BBOX(geom,",
      bbox["lon_min"], ",", bbox["lat_min"], ",",
      bbox["lon_max"], ",", bbox["lat_max"], ")"
    ),
    paste0(
      "https://gis.ices.dk/geoserver/ices/ows?service=WFS&version=1.0.0",
      "&request=GetFeature&typeName=ices:SweptAreaRatio",
      "&outputFormat=json",
      "&CQL_FILTER=BBOX(geom,",
      bbox["lon_min"], ",", bbox["lat_min"], ",",
      bbox["lon_max"], ",", bbox["lat_max"], ")"
    )
  )

  for (url in urls) {
    resp <- tryCatch(
      httr::GET(url, httr::timeout(45)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::http_error(resp)) next

    content_txt <- httr::content(resp, "text", encoding = "UTF-8")
    result <- tryCatch({
      geo   <- jsonlite::fromJSON(content_txt, flatten = TRUE)
      feats <- geo$features
      if (!is.null(feats) && nrow(feats) > 0) {
        df <- feats$properties
        coords <- do.call(rbind, lapply(feats$geometry$coordinates, function(x) {
          if (is.matrix(x)) c(lon = mean(x[,1]), lat = mean(x[,2]))
          else c(lon = x[[1]], lat = x[[2]])
        }))
        df$lon <- coords[, "lon"]
        df$lat <- coords[, "lat"]
        df
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(result) && nrow(result) > 0) {
      if (verbose)
        cli::cli_inform("    ICES VMS: {nrow(result)} c-square{?s} retrieved.")
      return(result)
    }
  }
  NULL
}

.fetch_emodnet_predators <- function(bbox, verbose) {
  .check_httr()
  if (verbose) cli::cli_inform("  Fetching EMODnet predator occurrences...")

  # EMODnet Biology WFS \u2014 Asterias rubens (starfish), Carcinus maenas (green crab)
  # AphiaIDs: Asterias rubens = 123776, Carcinus maenas = 107381
  species_ids <- c("123776", "107381")  # Asterias rubens, Carcinus maenas

  fetch_species <- function(aphia_id) {
    # EMODnet Biology occurrence data via the VLIZ WFS endpoint.
    # Layer updated to the current EMODnet Biology full occurrence dataset.
    url <- paste0(
      "https://geo.vliz.be/geoserver/Emodnetbio/wfs?",
      "SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature",
      "&TypeName=Emodnetbio:occurrence_species_positions_by_area",
      "&outputFormat=application/json",
      "&count=500",
      "&CQL_FILTER=aphiaid=", aphia_id,
      "%20AND%20BBOX(the_geom,",
      bbox["lon_min"], ",", bbox["lat_min"], ",",
      bbox["lon_max"], ",", bbox["lat_max"], ")"
    )
    resp <- tryCatch(
      httr::GET(url, httr::timeout(30)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::http_error(resp)) return(NULL)
    tryCatch({
      geo <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                                flatten = TRUE)
      feats <- geo$features
      if (!is.null(feats) && nrow(feats) > 0) {
        df <- feats$properties
        coords <- do.call(rbind, lapply(feats$geometry$coordinates, function(x) x))
        df$lon <- coords[, 1]; df$lat <- coords[, 2]
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

  # FSA/CEFAS England & Wales classified shellfish harvesting areas.
  # Primary: CEFAS ArcGIS Online feature service (replaced EA ArcGIS in 2024).
  # Fallback: original environment.data.gov.uk endpoint (slow but still live).
  urls <- list(
    list(
      url = paste0(
        "https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/",
        "Classified_Shellfish_Harvesting_Areas/FeatureServer/0/query?",
        "geometry=", bbox["lon_min"], ",", bbox["lat_min"], ",",
        bbox["lon_max"], ",", bbox["lat_max"],
        "&geometryType=esriGeometryEnvelope&spatialRel=esriSpatialRelIntersects",
        "&outFields=*&f=json&resultRecordCount=200"
      ),
      timeout = 20
    ),
    list(
      url = paste0(
        "https://environment.data.gov.uk/arcgis/rest/services/EA/",
        "ShellFishHarvestingAreas/MapServer/0/query?",
        "geometry=", bbox["lon_min"], ",", bbox["lat_min"], ",",
        bbox["lon_max"], ",", bbox["lat_max"],
        "&geometryType=esriGeometryEnvelope",
        "&spatialRel=esriSpatialRelIntersects",
        "&outFields=*&f=json"
      ),
      timeout = 60
    )
  )

  resp <- NULL
  for (u in urls) {
    resp <- tryCatch(
      httr::GET(u$url, httr::timeout(u$timeout)),
      error = function(e) NULL
    )
    if (!is.null(resp) && !httr::http_error(resp)) break
    resp <- NULL
  }
  if (is.null(resp))
    return(NULL)

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
