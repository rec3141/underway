#!/usr/bin/env Rscript
# Tasks:
# 1) timeseries plots
# 1.1) add option for x-axis to be distance instead of time
# 1.2) add multiple distance options (e.g. 10km 100km 200km 500km 1000km all)
# 1.3) (stretch goal) add ability for users to plot custom x-axis ranges (may require interactive e.g. plotly)
# 1.4) fix line plots so that missing data is missing without a connection line between them
# 1.5) sane scaling of y-axis when the data is very spiky with a few large outliers (e.g. fluorescence - maybe log10 it?)
# 2) map plots
# 2.1) add station location data as points
# 2.2) (stretch goal) include CTD cast data, as a popup or even a third plot
# 2.3) re 1.4 maps should maintain the missing data as a visible white line
# 3) aesthetics
# 3.1) fix font-size mismatch between browsers (e.g. in javascript outputs)
# 3.2) other cleanups to make nice (e.g. config$lists for constants)
# 4) codebase
# 4.1) clean up the code, remove unused or overly complex functions (e.g. do we really need 3 surprise formulas)
# 4.2) nicely format and comment the code for future maintenance
# 4.3) write a manual on how to set up and run the software, including use cases
# 
# Dashboard with combined Time Series + Leaflet Map + new "Surprise (−log10 p)" panel.
# - Reads ACSD_YYYYMMDD.csv (semicolon; two-line headers: instrument + variable)
# - Concats headers as "<instrument> — <variable>" (unique)
# - Builds rolling-window plots (1h,3h,6h,12h,24h,ALL)
# - For each variable: colored X‑Y + colored cruise‑track map with hover/click
# - Adds derived variable: Surprise (−log10 p) computed on 1‑minute aggregates
#   using PCA (T^2 & Q/SPE) + shrinkage Mahalanobis with empirical p-values
#
# Usage:
#   Rscript underway_dashboard_with_surprise.R \
#     --input  /path/to/share \
#     --output /path/to/share/underway_dashboard \
#     --default-window 6h \
#     --learn-hours 24
#
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(lubridate); library(tidyr)
  library(ggplot2); library(scales); library(htmltools); library(tools); library(sf);
  library(leaflet); library(leaflet.extras); library(htmlwidgets); library(viridis); library(corpcor); library(cowplot);
  library(gargle); library(httr2); library(jsonlite); library(blastula); library(geosphere)

  })

# ------------------------ CLI ------------------------
setwd("/mnt/ship/Share/2026/2026_LEG_03/Collins/")
args <- commandArgs(trailingOnly = TRUE)
INPUT_DIR  <- "/mnt/ship/Data/FULL_CSV/2026_LEG_03/"
# --indir may be repeated, or given a comma-separated list, to draw on more
# than one leg or season at once.
INPUT_DIRS <- character()
OUTPUT_DIR <- tempdir()
WEB_DIR <- file.path(getwd(),"underway_dashboard/")
DEFAULT_WIN <- "6h"
TESTING <- FALSE
WIN_OPTIONS <- c("1h","3h","6h","12h","24h","48h","72h","ALL")
#WIN_OPTIONS <- c("6h","24h")
LEARN_HOURS <- 48       # surprise model fit window
MIN_FEAT_COVER <- 0.60  # drop features below 60% finite coverage in learn window
LOCAL_TZ = "America/Toronto" #setting is explicitly because my cron defaults to UTC
MAP_MAX_SEGS <- 10000   # cap segments per map (tune)
POPUP_EVERY  <- 1     # only 1/N segments get popups (rest are hoverless)

X_MODES <- c("time","distance")
DEFAULT_X_MODE <- "time"

X_MODE <- "time"   # "time" | "distance"
COLOR_MODE <- "value"  # "value" | "hour"   (for request #1)
STATION_CSV <- file.path("/mnt/ship/Data/Rosette/2026_LEG_03/Logs/2026_03_CTD_logbook.csv")

CACHE_DIR <- path.expand("~/Desktop/underway_dev/cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
CACHE_INDEX <- file.path(CACHE_DIR, ".cache_index.rds")

if (length(args) > 0) {
  for (i in seq(1, length(args), by = 2)) {
    key <- args[i]
    val <- if (i + 1 <= length(args)) args[i + 1] else ""
    if (key %in% c("--indir","-i")) INPUT_DIRS <- c(INPUT_DIRS, trimws(strsplit(val, ",")[[1]]))
    if (key %in% c("--outdir","-o")) OUTPUT_DIR <- val
    if (key %in% c("--testing","-t")) TESTING <- TRUE
    if (key %in% c("--x-mode")) X_MODE <- tolower(val)   # "time" or "distance"
    if (key %in% c("--color-by")) COLOR_MODE <- tolower(val)  # "value" or "hour"
    if (key %in% c("--stations")) STATION_CSV <- val
  }
}

if (!length(INPUT_DIRS)) INPUT_DIRS <- INPUT_DIR
INPUT_DIRS <- unique(INPUT_DIRS[nzchar(INPUT_DIRS)])
INPUT_DIR  <- INPUT_DIRS[1]

if (!DEFAULT_WIN %in% WIN_OPTIONS) DEFAULT_WIN <- "6h"
if (TESTING) WIN_OPTIONS <- c("48h")

PNG_DIR <- file.path(OUTPUT_DIR, "pngs"); dir.create(PNG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(PNG_DIR, "libs"), recursive = TRUE, showWarnings = FALSE) # for leaflet libdir

message("Input:  ", INPUT_DIR)
message("Output: ", OUTPUT_DIR)
message("Default window: ", DEFAULT_WIN)

# ---- Color & scaling knobs (single source of truth) ----
VIRIDIS_OPT <- "C"   # A, B, C, D, E, F, G…
VIRIDIS_DIR <-  1    # 1 = low→high (purple→yellow), -1 = reversed
USE_QUANTILE_LIMITS <- TRUE   # TRUE: clamp to central quantiles for both plots/legends
QUANTILES <- c(0.05, 0.95)    # same clamping in ggplot & leaflet legends

make_palettes <- function(values, label) {
  finite_vals <- values[is.finite(values)]
  if (length(finite_vals)) {
    if (USE_QUANTILE_LIMITS) {
      rng <- quantile(finite_vals, QUANTILES, na.rm = TRUE, names = FALSE)
    } else {
      rng <- range(finite_vals)
    }
  } else {
    rng <- c(NA_real_, NA_real_)
  }
  # A variable can be wholly absent from a window, or constant across it, when
  # the sources span several seasons. leaflet::colorNumeric() cannot build a
  # scale from an empty or zero-width domain, so give it a usable one.
  if (!all(is.finite(rng))) rng <- c(0, 1)
  if (rng[1] == rng[2])     rng <- rng + c(-0.5, 0.5)
  cols <- viridis::viridis(256, option = VIRIDIS_OPT, direction = VIRIDIS_DIR)

  list(
    limits = rng,
    gg_scale = ggplot2::scale_color_gradientn(colours = cols, limits = rng, 
                                              oob = scales::squish, name = label),
    lf_pal   = leaflet::colorNumeric(cols, domain = rng, na.color = "transparent")
  )
}

# Cyclic palette for hour-of-day (0..24 wraps to 0)
make_hour_palette_cyclic <- function(h_start = 240, chroma = 60, luminance = 65) {
  # start hue at 240° (blue-ish midnight), wrap 360° back to the same color
  hues <- (seq(h_start, h_start + 360, length.out = 361)) %% 360
  cols <- grDevices::hcl(h = hues, c = chroma, l = luminance)   # smooth perceptual wheel
  list(
    limits  = c(0, 24),
    gg_scale = ggplot2::scale_color_gradientn(
      colours = cols, limits = c(0, 24),
      oob = scales::squish, name = "Hour"
    ),
    lf_pal = leaflet::colorNumeric(
      palette = cols, domain = c(0, 24), na.color = "transparent"
    )
  )
}



# ------------------------ Helpers ------------------------
mirror_input_to_cache <- function(src_dirs, cache_dir, index_path) {
  pat <- "^ACSD_\\d{8}\\.csv$"
  src <- unlist(lapply(src_dirs, function(d)
    list.files(d, pattern = pat, full.names = TRUE)), use.names = FALSE)
  if (!length(src)) return(character(0))
  # A date present in more than one source resolves to the earliest directory
  # listed, so the order of --indir decides precedence.
  src <- src[!duplicated(basename(src))]
  
  # current state in source (metadata only; no content read)
  info <- file.info(src)
  df_src <- data.frame(
    src_path = src,
    file = basename(src),
    size = as.numeric(info$size),
    mtime = as.POSIXct(info$mtime, tz = "UTC"),
    stringsAsFactors = FALSE, row.names = NULL
  )
  
  # load previous index (what's already mirrored)
  idx_old <- if (file.exists(index_path)) readRDS(index_path) else
    data.frame(file = character(), size = numeric(), mtime = as.POSIXct(character()), stringsAsFactors = FALSE)
  
  # find changed / new files
  idx_join <- merge(df_src[, c("file","size","mtime")], idx_old, by = "file", all.x = TRUE, suffixes = c("", ".old"))
  to_copy <- with(idx_join, is.na(size.old) | size != size.old | mtime > mtime.old)
  changed <- df_src$file[to_copy]
  
  # copy changed files
  if (length(changed)) {
    # sources may span several directories, so copy from the recorded path
    from <- df_src$src_path[match(changed, df_src$file)]
    ok <- file.copy(from, file.path(cache_dir, changed), overwrite = TRUE, copy.mode = TRUE)
    if (!all(ok)) warning("Some files failed to copy to cache: ", paste(changed[!ok], collapse = ", "))
  }
  
  # remove cache files that no longer exist upstream (optional)
  cache_files <- list.files(cache_dir, pattern = pat, full.names = FALSE)
  stale <- setdiff(cache_files, df_src$file)
  if (length(stale)) unlink(file.path(cache_dir, stale))
  
  # write new index
  saveRDS(df_src[, c("file","size","mtime")], index_path)
  
  # return the list of cached files (full paths)
  list.files(cache_dir, pattern = pat, full.names = TRUE)
}

mirror_with_rsync <- function(src_dir, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cmd <- sprintf('rsync -rtu --size-only --exclude=".*" --include="ACSD_????????.csv" --exclude="*" "%s/" "%s/"',
                 normalizePath(src_dir, mustWork = TRUE),
                 normalizePath(cache_dir, mustWork = TRUE))
  code <- system(cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
  if (code != 0) warning("rsync returned non-zero exit code: ", code)
  list.files(cache_dir, pattern = "^ACSD_\\d{8}\\.csv$", full.names = TRUE)
}

read_underway_cached <- function(csv_path) {
  rds_path <- sub("\\.csv$", ".rds", file.path(CACHE_DIR, basename(csv_path)))
  if (file.exists(rds_path)) {
    # simple freshness check: rds newer than csv
    if (file.info(rds_path)$mtime >= file.info(csv_path)$mtime) {
      return(readRDS(rds_path))
    }
  }
  df <- read_underway_csv(csv_path)      # your existing robust CSV reader
  saveRDS(df, rds_path)
  df
}

trim_collapse <- function(x) trimws(gsub("\\s+", " ", x))
is_data_line <- function(s) grepl("^\\s*\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}", s)

compose_colnames <- function(line1, line2) {
  l1 <- strsplit(line1, ";", fixed = TRUE)[[1]]
  l2 <- if (length(line2)) strsplit(line2, ";", fixed = TRUE)[[1]] else character(0)
  len <- max(length(l1), length(l2))
  l1 <- c(l1, rep("", len - length(l1))); l2 <- c(l2, rep("", len - length(l2)))
  l1 <- trim_collapse(l1); l2 <- trim_collapse(l2)
  combo <- ifelse(nchar(l2) == 0, l1, paste0(l2, " — ", l1))
  make.unique(trimws(combo), sep = ".")
}

read_underway_csv <- function(path) {
  hdr <- suppressWarnings(readr::read_lines(path, n_max = 2, progress = FALSE))
  cat(".")
  if (length(hdr) >= 2 && !is_data_line(hdr[2])) {
    coln <- compose_colnames(hdr[1], hdr[2])
    df <- suppressWarnings(read_delim(
      file = path, delim = ";", skip = 2, col_names = coln, 
      na = c("NaN","NA","","Inf","-Inf"),
      col_types = cols(.default = col_character()),
      trim_ws = TRUE, progress = FALSE
    ))
  } else {
    df <- suppressWarnings(read_delim(
      file = path, delim = ";",
      col_types = cols(.default = col_character()),
      na = c("NaN","NA","","Inf","-Inf"),
      trim_ws = TRUE, progress = FALSE
    ))
    names(df) <- trim_collapse(names(df))
  }
  
  df$.source_file <- basename(path)
  df
}

parse_time_any <- function(df) {
  nms <- names(df)
  hits <- grep("(?i)time.*yyyy|time \\(yyyy", nms, perl = TRUE, value = TRUE)
  if (length(hits) == 0) hits <- grep("(?i)\\b(time|timestamp|date ?time|utc)\\b", nms, perl = TRUE, value = TRUE)
  if (length(hits) == 0) hits <- nms[1]
  t <- suppressWarnings(lubridate::ymd_hms(df[[hits[1]]], tz = "UTC"))
  bad <- is.na(t)
  if (any(bad)) t[bad] <- suppressWarnings(lubridate::ymd_hm(df[[hits[1]]], tz = "UTC"))[bad]
  t
}

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x2 <- stringr::str_replace_all(x, ",", ".")
  x2 <- stringr::str_replace_all(x2, "[^0-9eE\\+\\-\\.]", " ")
  x2 <- stringr::str_squish(x2)
  xnum <- suppressWarnings(as.numeric(x2))
  xnum[is.nan(xnum) | is.infinite(xnum)] <- NA_real_   # <— add this line
  xnum
}

is_constant <- function(v) {
  v <- v[is.finite(v)]
  if (length(v) <= 1) return(TRUE)
  (max(v) - min(v)) == 0
}

safe_label <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

theme_underway <- theme_minimal(base_family = "sans", base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.5, colour = "grey85"),
    panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey85"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    plot.margin = margin(6, 14, 6, 6),
    legend.position = "right"
  )

plot_with_fixed_legend <- function(p, legend_title = "Value",
                                   rel_legend_width = 0.22,  # 22% of figure for legend (tweak)
                                   bar_h = 110, bar_w = 10) {
  p_leg <- p +
    guides(color = guide_colorbar(title = legend_title,
                                  barheight = grid::unit(bar_h, "pt"),
                                  barwidth  = grid::unit(bar_w, "pt"))) +
    theme(legend.position = "right")
  
  g_leg   <- cowplot::get_legend(p_leg)
  p_noleg <- p + guides(color = "none", fill = "none")
  
  cowplot::plot_grid(p_noleg, g_leg, ncol = 2,
                     rel_widths = c(1, rel_legend_width), align = "h")
}

fast_leaflet_segments <- function(map_data, label, pal, prefer_canvas = TRUE, stations = NULL) {
  # Downsample segments evenly if needed
  n <- nrow(map_data)
  if (n < 2) {
    return(leaflet(options = leafletOptions(preferCanvas = prefer_canvas)) %>% addTiles())
  }
  idx <- if (n > 1) seq_len(n - 1L) else integer(0)
  if (length(idx) > MAP_MAX_SEGS) {
    idx <- unique(round(seq(1, length(idx), length.out = MAP_MAX_SEGS)))
  }
  if (!length(idx)) return(NULL)
  
  s1 <- map_data[idx, ]
  s2 <- map_data[idx + 1L, ]
  
  lf_pal <- pal$lf_pal
  
  # Build an sf multilinestring object (1 row per short segment)
  segs <- mapply(function(x1,y1,x2,y2) {
    sf::st_linestring(matrix(c(x1, x2, y1, y2), ncol = 2))
  }, s1$lon, s1$lat, s2$lon, s2$lat, SIMPLIFY = FALSE)
  
  seg_sf <- sf::st_sf(
    value = s1$value,                                           # driver for color
    value_squish = s1$value %>% oob_squish(range = pal$limits), # squish within palette limits
    tstr  = format(s1$time_utc, "%Y-%m-%d %H:%M UTC"),
    lat1  = round(s1$lat, 4), lon1 = round(s1$lon, 4),
    geometry = sf::st_sfc(segs, crs = 4326)
  )
  
  # Popups only on every POPUP_EVERY-th segment
  seg_sf$popup <- ifelse((seq_len(nrow(seg_sf)) %% POPUP_EVERY) == 0,
                         sprintf("<b>%s:</b> %s<br><b>Time:</b> %s<br><b>Pos:</b> %.4f°, %.4f°",
                                 label, signif(seg_sf$value, 2), seg_sf$tstr, seg_sf$lat1, seg_sf$lon1),
                         NA)
  
  # m <- leaflet(options = leafletOptions(preferCanvas = prefer_canvas)) %>% addTiles()
  m <- leaflet(options = leafletOptions(preferCanvas = TRUE), width = "100%", height = 480) %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  m <- m %>%
    addPolylines(data = seg_sf,
                 color = ~lf_pal(value_squish), weight = 10, opacity = 0.95,
                 popup = ~popup, smoothFactor = 0, options = pathOptions())
   
  # --- Station markers on top (sized & colored by type) ---
  if (!is.null(stations) && nrow(stations)) {
    st_ok <- stations
    m <- m %>%
      addCircleMarkers(
        data = st_ok,
        lng = ~longitude, lat = ~latitude,
        radius = ~radius,                         # <- sized
        weight = 2, stroke = TRUE,
        color = "#111111",                        # stroke (dark gray/black)
        fillColor = ~fill_color,                  # <- colored
        fillOpacity = 1,
        label = ~station,
        popup = ~sprintf("<b>Cast %s</b><br/>Type: %s<br/>Date: %s<br/>Bottom Depth: %s<br/>Comments: %s", station, htmltools::htmlEscape(type_desc), htmltools::htmlEscape(date_UTC), htmltools::htmlEscape(bottom_m), htmltools::htmlEscape(comments)),
        options = pathOptions(pane = "markerPane")
      )
  }
  
  
  m <- m %>%
    addCircleMarkers(lng = s1$lon[1], lat = s1$lat[1], radius = 6,
                     color = "red", fillColor = "pink", fillOpacity = 0.8,
                     popup = "Track Start", options = pathOptions(pane = "markerPane")) %>%
    addCircleMarkers(lng = s2$lon[nrow(s2)], lat = s2$lat[nrow(s2)], radius = 6,
                     color = "green", fillColor = "lightgreen", fillOpacity = 0.8,
                     popup = "Track End", options = pathOptions(pane = "markerPane")) %>%
    fitBounds(lng1 = min(c(s1$lon, s2$lon)) - 0.01,
              lat1 = min(c(s1$lat, s2$lat)) - 0.01,
              lng2 = max(c(s1$lon, s2$lon)) + 0.01,
              lat2 = max(c(s1$lat, s2$lat)) + 0.01) %>% 
    addLegend(pal = lf_pal, values = map_data$value, title = NULL, position = "bottomright", opacity = 1)
  
    return(m)
}

cumulative_distance_geo <- function(lats, lons, units = "km") {
  # Input validation
  if (length(lats) != length(lons)) {
    stop("Latitude and longitude vectors must be the same length")
  }
  
  if (length(lats) < 2) {
    return(numeric(length(lats)))  # Return zeros for 0 or 1 points
  }
  
  # Remove NA values
  valid_idx <- !is.na(lats) & !is.na(lons)
  if (sum(valid_idx) < 2) {
    return(rep(NA_real_, length(lats)))
  }
  
  # Create coordinate matrix (longitude, latitude - note the order!)
  coords <- cbind(lons, lats)
  
  # Calculate distances between consecutive points
  distances <- numeric(length(lats))
  distances[1] <- 0  # Starting point has 0 cumulative distance
  
  for (i in 2:length(lats)) {
    if (valid_idx[i-1] && valid_idx[i]) {
      # distHaversine returns distance in meters
      dist_m <- geosphere::distHaversine(coords[i-1, ], coords[i, ])
      
      # Convert units
      dist_converted <- switch(units,
                               "m" = dist_m,
                               "km" = dist_m / 1000,
                               "nm" = dist_m / 1852,  # nautical miles
                               "mi" = dist_m / 1609.344,  # statute miles
                               dist_m / 1000  # default to km
      )
      
      distances[i] <- distances[i-1] + dist_converted
    } else {
      distances[i] <- ifelse(i > 1, distances[i-1], 0)  # Carry forward if NA
    }
  }
  
  distances
}


# want_patterns <- list(
#   "Ship Speed (knt)" = "(?i)\\b(SOG|Speed|Speed over ground).*?(knt|kts|kn|knots|kt|m/s)\\b",
#   "Heading (deg)"    = "(?i)\\b(Heading|COG|Course).*?(deg|°)\\b",
#   "SST (°C)"         = "(?i)(^|—)\\s*(temper|SST|Sea.*Temp|Surface.*Temp).*°?C(?!.*(diff|delta|offset|anom|resid|qc|flag))",
#   "Salinity (PSU)"   = "(?i)(^|—)\\s*Salinity\\s*\\(PSU\\)\\s*(?!.*(diff|delta|offset|anom|resid|qc|flag))",
#   "Conductivity"     = "(?i)conductiv(?!.*(flag|qc))",
#   "Fluorescence"     = "(?i)fluor(?!.*(flag|qc))",
#   "Turbidity"        = "(?i)turbid|\\bNTU\\b"
# )

#it will use this order if available
want_patterns <- list(
  "Surprise (−log10 p)" = NULL,
  "SST (°C)"         = "TSG — Hull temperature (deg C)",
  "Salinity (PSU)"   = "TSG — Salinity (psu)",
  "Fluorescence"     = "TSG — Fluorescence (ug/L)",
  "Short wave radiation (W/m²)" = "ATS_Portside — Short wave radiation (W/m²)",
  "Bottom Depth (m)" = "Multibeam — Bottom depth (m)",
  "Time Elapsed (h)"            = NULL,
  "Distance Traveled (km)" = NULL,
  "Air Temperature (C)" = "AVOS — Air temperature (deg C)",
#  "Air Humidity (%)" = "AVOS — Air humidity (%)",
  "True Wind Direction (deg)" = "AVOS — True wind direction (deg)",
  "Relative Wind Speed (knt)" = "AVOS — Relative wind speed (knt)",
  "Ship Speed (knt)" = "POSMV — Speed (knt)"
#  "Heading (deg)"    = "POSMV — Heading (deg)"
)

# add depth, time plots

prefer_abs <- function(hits) {
  if (!length(hits)) return(hits)
  score <- rep(0, length(hits))
  score <- score - nchar(hits) * 0.01
  hits[order(-score)]
}

resolve_columns <- function(df, patterns) {
  out <- list()
  for (label in names(patterns)) {
    patt <- patterns[[label]]
    #    hits <- grep(patt, names(df), value = TRUE, ignore.case = TRUE, perl = TRUE)
    hits <- names(df)[which(names(df) %in% patt)]
    # clean <- hits[!grepl(DEMOTE, hits, perl=TRUE)]
    hits <- prefer_abs(hits)
    for (h in hits) {
      v <- to_num(df[[h]])
      if (!all(is.na(v)) && !is_constant(v)) { out[[label]] <- h; break }
    }
  }
  out
}

make_hour_palette <- function() {
  cols <- viridis::viridis(256, option = "A", direction = 1)
  list(
    limits = c(0,24),
    gg_scale = ggplot2::scale_color_gradientn(colours = cols, limits = c(0,24), oob = scales::squish, name = "Hour"),
    lf_pal   = leaflet::colorNumeric(cols, domain = c(0,24), na.color = "transparent")
  )
}


# ------------------------ Load data ------------------------
# Mirror network -> local cache (only changed files)
# usage:
# then:
files <- mirror_input_to_cache(INPUT_DIRS, CACHE_DIR, CACHE_INDEX)  # or mirror_with_rsync()

#files <- mirror_with_rsync(INPUT_DIR, CACHE_DIR)
#files <- mirror_input_to_cache(INPUT_DIR, CACHE_DIR, CACHE_INDEX)
if (length(files) == 0) {
  message("No ACSD_YYYYMMDD.csv files found in: ", paste(INPUT_DIRS, collapse = ", "))
  quit(status = 1)
}
files <- sort(files)

message(sprintf("Importing %d files (cached in %s)", length(files), CACHE_DIR))

dfs <- lapply(files, read_underway_csv)
cat("\n")
for (i in seq_along(dfs)) dfs[[i]]$time_utc <- parse_time_any(dfs[[i]])
df_all <- bind_rows(dfs) %>% filter(!is.na(time_utc)) %>% arrange(time_utc)
df_all$minute <- floor_date(df_all$time_utc, "1 minute")
df_all$time_elapsed <- (as.numeric(df_all$minute) - min(as.numeric(df_all$minute)))/60/60
df_all$hour_local <- lubridate::hour(lubridate::with_tz(df_all$time_utc, tzone = LOCAL_TZ))
df_all$latitude <- as.numeric(df_all$`POSMV — Latitude (deg N)`)
df_all$longitude <- as.numeric(df_all$`POSMV — Longitude (deg E)`)
df_all$distance_traveled <- cumulative_distance_geo(df_all$latitude, df_all$longitude, units = "km")
  
# Numeric coercion
df_num <- df_all %>% mutate(across(.cols = -c(time_utc, minute), .fns = to_num))

# Identify lon/lat columns (prefer POSMV)
lon_pref <- grep("(?i)^POSMV.*lon|lon(gitude)?", names(df_num), value = TRUE)
lat_pref <- grep("(?i)^POSMV.*lat|lat(itude)?", names(df_num), value = TRUE)
lon_col <- lon_pref[1]; lat_col <- lat_pref[1]
if (is.na(lon_col) || is.na(lat_col)) {
  lon_col <- grep("(?i)lon(gitude)?", names(df_num), value = TRUE)[1]
  lat_col <- grep("(?i)lat(itude)?", names(df_num), value = TRUE)[1]
}

# Resolve variables (based on all data)
resolved <- resolve_columns(df_num, want_patterns)
resolved$`Time Elapsed (h)` <- "time_elapsed"
resolved$`Distance Traveled (km)` <- "distance_traveled"

# --- Stations: robust import + normalization + typing ---
read_stations <- function(path) {
  if (!file.exists(path)) return(NULL)
  s <- tryCatch(suppressWarnings(readr::read_csv(
    path, na = c("NA","","NaN","Inf","-Inf"), show_col_types = FALSE)),
    error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(NULL)
  
  # normalize column names
  names(s) <- trimws(names(s))
  
  c_station <- "station"
  c_lat     <- "latitude"
  c_lon     <- "longitude"
  c_type    <- "type_cast"
  c_com     <- "comments"
  c_date    <- "date_UTC"
  c_bot     <- "bottom_m"
  
  to_num_local <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    x2 <- stringr::str_replace_all(as.character(x), ",", ".")
    x2 <- stringr::str_replace_all(x2, "[^0-9eE\\+\\-\\.]", " ")
    x2 <- stringr::str_squish(x2)
    out <- suppressWarnings(as.numeric(x2))
    out[is.nan(out) | is.infinite(out)] <- NA_real_
    out
  }

  out <- data.frame(
    station    = s[[c_station]],
    latitude   = to_num_local(s[[c_lat]]),
    longitude  = to_num_local(s[[c_lon]]),
    type_desc  = s[[c_type]],
    date_UTC   = s[[c_date]],
    comments   = s[[c_com]],
    bottom_m   = s[[c_bot]],
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$latitude) & is.finite(out$longitude), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  
  # --- classify by regex (handles Classic-CTD/Nutrient/Full variants)
  norm <- function(z) tolower(gsub("[^a-zA-Z]+"," ", z))
  d <- norm(out$type_desc)
  is_classic <- grepl("classic", d)
  has_full   <- grepl("full", d)
  has_nutr   <- grepl("nutr", d)       # nutrient / nutr / -nutrient
  has_ctd    <- grepl("ctd", d)

  out$cast_type <- ifelse(is_classic & has_full,   "Classic Full",
                          ifelse(is_classic & has_nutr,   "Classic Nutrient",
                                 ifelse(is_classic & has_ctd,    "Classic CTD",
                                        "Other")))
  
  # sizes
  base_r <- 5
  out$radius <- base_r + ifelse(out$cast_type == "Classic Full", 10,
                                ifelse(out$cast_type == "Classic Nutrient", 5, 2))
  
  # colors
  col_map <- c(
    "Classic CTD"      = "#FFD400",  # yellow
    "Classic Nutrient" = "#E02424",  # red
    "Classic Full"     = "#22A559",  # green
    "Other"            = "#FFFFFF"   # white
  )
  out$fill_color <- unname(col_map[ ifelse(out$cast_type %in% names(col_map), out$cast_type, "Other") ])
  
  # quick log so you can verify
  msg <- tryCatch(capture.output(print(as.data.frame(table(out$cast_type)))), error=function(e) NULL)
  if (!is.null(msg)) message("Station type counts:\n", paste(msg, collapse="\n"))
  
  out
}


stations <- read_stations(STATION_CSV)

# ------------------------ Surprise score (1‑min) ------------------------
# Build a robust minute-level table, fit model on last LEARN_HOURS
feature_cols <- unname(unlist(resolve_columns(df_num, want_patterns[c("SST (°C)","Salinity (PSU)","Fluorescence")])))
if (length(feature_cols) >= 2) {
  
  # minute aggregation
  df_min <- df_num %>%
    group_by(minute) %>%
    summarize(
      across(all_of(feature_cols), ~ suppressWarnings(median(.x, na.rm = TRUE)), .names = "{.col}"),
      lon = if (!is.na(lon_col)) mean(.data[[lon_col]], na.rm = TRUE) else NA_real_,
      lat = if (!is.na(lat_col)) mean(.data[[lat_col]], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>% arrange(minute)
  
  # learn window = last LEARN_HOURS hours
  t_cut <- max(df_min$minute, na.rm = TRUE) - hours(LEARN_HOURS)
  df_learn <- df_min %>% filter(minute >= t_cut & minute <= max(df_min$minute, na.rm=TRUE))
  
  # feature coverage filter
  feat_cover <- sapply(df_learn[, feature_cols, drop=FALSE], function(v) mean(is.finite(v)))
  feature_cols2 <- names(feat_cover)[feat_cover >= MIN_FEAT_COVER]
  if (length(feature_cols2) >= 2) {
    winsor <- function(v, p = c(0.005, 0.995)) {
      f <- is.finite(v); if (sum(f) < 10) return(v)
      qs <- quantile(v[f], probs = p, names = FALSE)
      pmin(pmax(v, qs[1]), qs[2])
    }
    
    X_learn <- df_learn %>% select(all_of(feature_cols2)) %>% mutate(across(everything(), winsor))
    med <- apply(X_learn, 2, median, na.rm = TRUE)
    iqr <- apply(X_learn, 2, IQR, na.rm = TRUE); iqr[!is.finite(iqr) | iqr == 0] <- 1
    scale_apply <- function(df) as.data.frame(mapply(function(col, m, s) (col - m)/s, df, med, iqr, SIMPLIFY = FALSE))
    
    Xz_learn_pre <- scale_apply(X_learn)
    X_all <- df_min %>% select(all_of(feature_cols2)) %>% mutate(across(everything(), winsor))
    Xz_all <- scale_apply(X_all)
    
    # Keep only complete finite rows for model fitting (avoid SVD errors)
    Xz_learn_mat <- as.matrix(Xz_learn_pre)
    learn_ok <- stats::complete.cases(Xz_learn_mat) & apply(is.finite(Xz_learn_mat), 1, all)
    Xz_learn <- Xz_learn_mat[learn_ok, , drop = FALSE]
    if (nrow(Xz_learn) < 2) stop("Surprise: not enough finite rows for PCA")
    
    pc <- prcomp(as.matrix(Xz_learn), center = FALSE, scale. = FALSE)
    vr <- pc$sdev^2 / sum(pc$sdev^2)
    k <- max(1, which(cumsum(vr) <= 0.95)); if (k == length(vr)) k <- max(1, length(vr)-1)
    P <- pc$rotation[, 1:k, drop = FALSE]; eig <- pc$sdev[1:k]^2
    
    good_rows <- stats::complete.cases(Xz_all) & apply(is.finite(as.matrix(Xz_all)), 1, all)
    scores_all <- matrix(NA_real_, nrow(Xz_all), k)
    scores_all[good_rows, ] <- as.matrix(Xz_all[good_rows, , drop=FALSE]) %*% P
    T2_all <- rep(NA_real_, nrow(Xz_all))
    T2_all[good_rows] <- rowSums((scores_all[good_rows, , drop=FALSE]^2) / rep(eig, each = sum(good_rows)))
    
    P_disc <- pc$rotation[, (k+1):ncol(pc$rotation), drop = FALSE]
    Q_all <- rep(NA_real_, nrow(Xz_all))
    if (ncol(P_disc) > 0) {
      Xhat_good <- scores_all[good_rows, , drop=FALSE] %*% t(P)
      E_good <- as.matrix(Xz_all[good_rows, , drop=FALSE]) - Xhat_good
      Q_all[good_rows] <- rowSums(E_good^2)
    } else Q_all[good_rows] <- 0
    
    scores_learn <- as.matrix(Xz_learn) %*% P
    T2_learn <- rowSums((scores_learn^2) / rep(eig, each = nrow(scores_learn)))
    if (ncol(P_disc) > 0) {
      Xhat_learn <- scores_learn %*% t(P); E_learn <- as.matrix(Xz_learn) - Xhat_learn; Q_learn <- rowSums(E_learn^2)
    } else Q_learn <- rep(0, nrow(Xz_learn))
    
    ecdf_p_upper <- function(train_vals, x) { f <- is.finite(train_vals); 1 - ecdf(train_vals[f])(x) + 1/sum(f) }
    p_T2 <- rep(NA_real_, length(T2_all)); p_Q <- rep(NA_real_, length(Q_all))
    p_T2[good_rows] <- ecdf_p_upper(T2_learn, T2_all[good_rows])
    p_Q[good_rows]  <- ecdf_p_upper(Q_learn,  Q_all[good_rows])
    
    S_shrink <- try(corpcor::cov.shrink(as.matrix(Xz_learn)), silent = TRUE)
    if (inherits(S_shrink, "try-error")) S_shrink <- cov(as.matrix(Xz_learn))
    mu <- colMeans(as.matrix(Xz_learn), na.rm = TRUE)
    MD_learn <- mahalanobis(as.matrix(Xz_learn), center = mu, cov = S_shrink)
    MD_all <- rep(NA_real_, nrow(Xz_all))
    MD_all[good_rows] <- mahalanobis(as.matrix(Xz_all[good_rows, , drop=FALSE]), center = mu, cov = S_shrink)
    p_MD <- rep(NA_real_, length(MD_all))
    p_MD[good_rows] <- ecdf_p_upper(MD_learn, MD_all[good_rows])
    
    #change from minimum to 
#    p_min <- pmin(p_T2, p_Q, p_MD, na.rm = TRUE)
#    surprise <- -log10(p_min); surprise[!is.finite(surprise)] <- NA_real_
    #smaller is more surprising
    p_sum <- rowSums(-log10(cbind(p_T2, p_Q, p_MD)), na.rm = TRUE)
    surprise <- p_sum; surprise[!is.finite(surprise)] <- NA_real_

    #bigger is more surprising
    df_min$surprise_score <- surprise
    # Join back to raw rows by minute
    df_num <- df_num %>% left_join(df_min[, c("minute","surprise_score")], by = "minute")
    resolved[["Surprise (−log10 p)"]] <- "surprise_score"
  } else {
    message("Surprise: not enough well-covered features for model; skipping panel.")
  }

} else {
  message("Surprise: fewer than 2 candidate features; skipping panel.")
}

# ------------------------ Window helpers ------------------------
cut_for_window <- function(df, win) {
  if (win == "ALL") return(df)
  hours <- as.numeric(gsub("h","",win))
  tmax <- max(df$time_utc, na.rm = TRUE)
  df %>% filter(time_utc >= tmax - lubridate::hours(hours))
}

x_breaks_for <- function(df_win, max_ticks = 8) {
  tmin <- min(df_win$time_utc, na.rm = TRUE)
  tmax <- max(df_win$time_utc, na.rm = TRUE)
  total_sec <- as.numeric(difftime(tmax, tmin, units = "secs"))
  if (!is.finite(total_sec) || total_sec <= 0) return(scales::date_breaks("1 hour"))
  
  # Candidate step sizes (in seconds)
  widths <- c(10*60, 30*60, 60*60, 2*3600, 3*3600, 6*3600, 12*3600,
              24*3600, 2*24*3600, 7*24*3600)
  
  # Smallest width giving <= max_ticks labels
  target <- total_sec / max_ticks
  w <- widths[which(widths >= target)][1]
  if (is.na(w)) w <- tail(widths, 1)
  
  spec <-
    if (w %% (7*24*3600) == 0) paste0(w/(7*24*3600), " week")
  else if (w %% (24*3600) == 0) paste0(w/(24*3600), " day")
  else if (w %% 3600 == 0) paste0(w/3600, " hour")
  else paste0(w/60, " min")
  
  scales::date_breaks(spec)
}

# Replace your helper with a vector version
x_breaks_numeric <- function(x, max_ticks = 8) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(pretty(rng[2], n = max_ticks))
  }
  scales::pretty_breaks(n = max_ticks)(rng)
}
x_labels_numeric <- function(x) sprintf("%.0f km", x)



x_labels_for <- function(df_win) {
  tmin <- min(df_win$time_utc, na.rm = TRUE)
  tmax <- max(df_win$time_utc, na.rm = TRUE)
  span <- as.numeric(difftime(tmax, tmin, units = "secs"))
  if (!is.finite(span)) span <- 0
  
  if (span >= 14*24*3600)      function(x) format(x, "%b %d", tz = "UTC")               # ≥2 weeks
  else if (span >= 3*24*3600)  function(x) format(x, "%b %d", tz = "UTC")               # 3–14 days
  else if (span >= 24*3600)    function(x) format(x, "%b %d\n%H:%M", tz = "UTC")        # 1–3 days
  else                         function(x) format(x, "%H:%M", tz = "UTC")               # < 1 day
}


# manifest rows

image_manifest <- data.frame(
  win = character(),
  x_mode = character(),                # <— NEW
  label = character(),
  timeseries_file = character(),
  map_file = character(),
  stringsAsFactors = FALSE
)

# Leaflet libdir must be descendant of html path
LIB_DIR_REL <- "libs"

# ------------------------ Plotting loop ------------------------
for (win in WIN_OPTIONS) {
  df_win <- cut_for_window(df_num, win)
  if (nrow(df_win) == 0) next
  xb <- x_breaks_for(df_win)
  tmin <- min(df_win$time_utc, na.rm = TRUE)
  tmax <- max(df_win$time_utc, na.rm = TRUE)
  time_sub <- paste0(format(tmin, "%Y-%m-%d %H:%M", tz = "UTC"), " to ",
                     format(tmax, "%Y-%m-%d %H:%M", tz = "UTC"), " (UTC)")
  
  for (label in names(resolved)) {
    colname <- resolved[[label]]
    if (!colname %in% names(df_win)) next
    

    # --------- Prepare data once ---------
    dat <- df_win %>%
      dplyr::select(time_utc, distance_traveled, hour_local,
                    value = dplyr::all_of(colname))
    
    # Ensure distance_traveled exists even when colname == "distance_traveled"
    if (!"distance_traveled" %in% names(dat)) {
      dat$distance_traveled <- if (identical(colname, "distance_traveled")) dat$value
      else df_win$distance_traveled
    }
    
    # (rest stays the same)
    use_hour        <- (COLOR_MODE == "hour" || identical(label, "Hour of Day"))
    dat$color_var   <- if (use_hour) dat$hour_local else dat$value
    legend_lab      <- if (use_hour) "Hour" else "Value"
    pal <- if (use_hour) make_hour_palette_cyclic() else make_palettes(dat$color_var, legend_lab)
    
    
    # Color choice (hour vs value) – keep whatever logic you use
    use_hour   <- (COLOR_MODE == "hour" || identical(label, "Hour of Day"))
    color_var  <- if (use_hour) dat$hour_local else dat$value
    legend_lab <- if (use_hour) "Hour" else "Value"
    pal <- if (use_hour) make_hour_palette_cyclic() else make_palettes(color_var, legend_lab)
    
    # --------- Build the MAP ONCE (independent of x-mode) ---------
    map_filename <- NA_character_
    if (!is.na(lon_col) && !is.na(lat_col) &&
        lon_col %in% names(df_win) && lat_col %in% names(df_win)) {
      
      map_data <- df_win %>%
        dplyr::select(time_utc,
                      lon = dplyr::all_of(lon_col),
                      lat = dplyr::all_of(lat_col),
                      value = dplyr::all_of(if (use_hour) "hour_local" else colname),  # driver matches palette
                      hour_local) %>%
        dplyr::filter(is.finite(lon), is.finite(lat), is.finite(value)) %>%
        dplyr::arrange(time_utc)
      
      if (nrow(map_data) > 2) {
        m <- fast_leaflet_segments(map_data, if (use_hour) "Hour of Day" else label,
                                   pal, stations = stations)
        map_filename <- file.path(PNG_DIR, paste0(safe_label(label),
                                                  "__map__w-", tolower(win), ".html"))
        htmlwidgets::saveWidget(m, file = map_filename, selfcontained = FALSE,
                                libdir = LIB_DIR_REL, title = paste(label, "Map"))
        map_filename <- basename(map_filename)  # store basename for manifest
      }
    }
    
    # --------- Now render BOTH X-MODES for the time series ---------
    for (x_mode in X_MODES) {
      if (identical(x_mode, "distance")) {
#        print(names(dat)); print(sum(is.finite(dat$distance_traveled)))
        
        p_ts <- ggplot2::ggplot(dat, ggplot2::aes(x = distance_traveled, y = value)) +
          ggplot2::geom_path(ggplot2::aes(color = color_var), linewidth = 2, na.rm = TRUE) +
          pal$gg_scale +
          ggplot2::scale_x_continuous(
            breaks = x_breaks_numeric(dat$distance_traveled),   # <— use the vector
            labels = x_labels_numeric,
            expand = ggplot2::expansion(mult = c(0.01, 0.01))
          ) +
          ggplot2::labs(title = resolved[label], x = "Distance (km)", y = label,
                        subtitle = time_sub, color = legend_lab) +
          theme_underway +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5, hjust = 1,
                                                margin = ggplot2::margin(t = 6)),
            plot.margin = ggplot2::margin(10, 14, 28, 6)   # bottom from 14 → 28
          )
        } else {
        p_ts <- ggplot2::ggplot(dat, ggplot2::aes(x = time_utc, y = value)) +
          ggplot2::geom_path(ggplot2::aes(color = color_var), linewidth = 2, na.rm = TRUE) +
          pal$gg_scale +
          ggplot2::scale_x_datetime(breaks = x_breaks_for(df_win),
                                    labels = x_labels_for(df_win),
                                    expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
          ggplot2::labs(title = resolved[label], y = label,
                        subtitle = time_sub, color = legend_lab) +
          theme_underway +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5, hjust = 1,
                                                margin = ggplot2::margin(t = 6)),
            plot.margin = ggplot2::margin(10, 14, 28, 6)
          )
        }
      
      p_fixed <- plot_with_fixed_legend(p_ts, legend_title = legend_lab,
                                        rel_legend_width = 0.22, bar_h = 110, bar_w = 10)
      ts_filename <- file.path(PNG_DIR, paste0(
        safe_label(label), "__timeseries__x-", x_mode, "__w-", tolower(win), ".png"
      ))
      ggsave(ts_filename, p_fixed, width = 8, height = 4.4, dpi = 150)
      
      # Manifest row for this (win, label, x_mode)
      image_manifest <- rbind(image_manifest, data.frame(
        win = win,
        x_mode = x_mode,
        label = label,
        timeseries_file = basename(ts_filename),
        map_file = if (!is.na(map_filename)) map_filename else NA_character_,
        stringsAsFactors = FALSE
      ))
    } # end for x_mode
    
    message("Created plots for ", label, " (", win, ")")
  } # end for label
  
  # --------- Ship Track Overview (map only, colored by hour) ---------
  if (!is.na(lon_col) && !is.na(lat_col) &&
      lon_col %in% names(df_win) && lat_col %in% names(df_win)) {
    coords <- df_win %>%
      dplyr::select(time_utc,
                    lon = dplyr::all_of(lon_col),
                    lat = dplyr::all_of(lat_col),
                    hour_local) %>%
      dplyr::filter(is.finite(lon), is.finite(lat)) %>%
      dplyr::arrange(time_utc)
    if (nrow(coords) > 2) {
      pal_hour <- make_hour_palette_cyclic()
      map_data <- coords %>% dplyr::mutate(value = hour_local)
      m_track <- fast_leaflet_segments(map_data, "Hour of Day", pal_hour, stations = stations)
      track_filename <- file.path(PNG_DIR, paste0("ship_track__w-", tolower(win), ".html"))
      htmlwidgets::saveWidget(m_track, file = track_filename, selfcontained = FALSE,
                              libdir = LIB_DIR_REL, title = "Ship Track")
      track_basename <- basename(track_filename)
      # Add a manifest row for each x_mode so UI toggle works
      for (x_mode in X_MODES) {
        image_manifest <- rbind(image_manifest, data.frame(
          win = win, x_mode = x_mode, label = "Ship Track Overview",
          timeseries_file = NA_character_,
          map_file = track_basename,
          stringsAsFactors = FALSE
        ))
      }
      message("Created ship track overview (", win, ")")
    }
  }
} # end for win


# ------------------------ HTML ------------------------
title <- paste0("CCGS Amundsen Underway Dashboard")
css <- "
body { font-size: 20px; font-family: system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif; margin: 24px; color: #111; background:#fafafa; }
 h1 { font-size: 22px; font-weight: 700; margin-bottom: 12px; }
 .meta { color:#555; margin-bottom: 12px; }
.controls { font-size: clamp(18px, 2.2vw, 24px); }
.controls select { font-size: inherit; }

 select { padding: 6px 10px; border-radius: 6px; border:1px solid #ccc; background:white; }

 .card { margin: 12px 0; }
 .card h3 { margin:6px 0 4px 0; font-weight:600; font-size:20px; }

 .combined-plot {
    display: grid;
    grid-template-columns: auto 1fr;
    gap: 16px;
    margin: 16px 0;
 }

 /* Media query for smaller screens remains the same and is correct. */
 @media (max-width: 1280px) { /* Increased breakpoint for better responsiveness */
    .combined-plot {
      grid-template-columns: 1fr; /* Stack vertically on smaller screens */
    }
 }

 .timeseries-container { min-width: 480px; }

.map-container {
  aspect-ratio: 2 / 1;    /* square map */
  min-width: 480px;
  /* no explicit height needed when using aspect-ratio */
}

 /* The iframe rule remains the same and is correct */
 .map-container iframe {
    width: 100%;
    height: 100%;
    border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.06);
 }

 .plot { display: none; }

 .timeseries-container img {
    /* KEY CHANGE 3: Set max-height and let width/height be auto for correct aspect ratio.
       width: 100% ensures it scales down if the container gets too narrow. */
    max-height: 480px;
    min-width: 480px;
    height: auto;
    width: 100%;
    object-fit: contain;
    border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.06);
 }

 @media (max-width: 480px) {
    .map-container { min-width: auto; }
 }
"

js <- htmltools::HTML("
(function(){
  function selWin(w){
    window._win = w;
    document.querySelectorAll('.plot').forEach(function(el){
      el.style.display = (el.dataset.win===w && el.dataset.x===window._xmode) ? 'block' : 'none';
    });
    var t = document.getElementById('winTitle');
    if (t) t.textContent = 'Selected Time Window: ' + w;
  }
  function selX(x){
    window._xmode = x;
    document.querySelectorAll('.plot').forEach(function(el){
      el.style.display = (el.dataset.win===window._win && el.dataset.x===x) ? 'block' : 'none';
    });
    var t = document.getElementById('xTitle');
    if (t) t.textContent = 'X: ' + (x==='time' ? 'Time' : 'Distance');
  }

  // expose for inline onchange= handlers (and we also bind listeners below)
  window.selWin = selWin;
  window.selX = selX;

  document.addEventListener('DOMContentLoaded', function(){
    var ws = document.getElementById('winSel');
    var xs = document.getElementById('xSel');

    // Initialize from URL hash if provided, else from current <select> values
    var xFromHash = (location.hash.match(/x=(time|distance)/)||[])[1];
    var wFromHash = (location.hash.match(/w=([A-Za-z0-9]+)/)||[])[1];

    window._xmode = xFromHash || (xs ? xs.value : 'time');
    window._win   = wFromHash || (ws ? ws.value : '6h');

    selX(window._xmode);   // set mode first
    selWin(window._win);   // then filter by window

    // Bind listeners (also prevent default to avoid any accidental navigation)
    if (xs) xs.addEventListener('change', function(e){ e.preventDefault(); selX(xs.value); });
    if (ws) ws.addEventListener('change', function(e){ e.preventDefault(); selWin(ws.value); });
  });
})();
")





cards <- list(
  tags$div(class="controls",
           "Select New Time Window: ",
           tags$select(
             id="winSel",
             onchange="window._win=this.value; selWin(this.value)",
             lapply(WIN_OPTIONS, function(w)
               tags$option(value=w, selected=if (identical(w, DEFAULT_WIN)) "selected" else NULL, w))
           ),
           tags$span(id="winTitle", style="font-size: 22px; margin-left:10px; color:#555;"),
           HTML("&nbsp;&nbsp;·&nbsp;&nbsp;"),
           "X-Axis: ",
           tags$select(
             id="xSel",
             onchange="selX(this.value)",     # <- IMPORTANT: no document.location here
             list(
               tags$option(value="time",     selected=if (DEFAULT_X_MODE=='time') "selected" else NULL, "Time"),
               tags$option(value="distance", selected=if (DEFAULT_X_MODE=='distance') "selected" else NULL, "Distance")
             )
           ),
           tags$span(id="xTitle", style="font-size: 22px; margin-left:10px; color:#555;")
  )
)




# Add combined plot cards
# Add combined plot cards
for (win in WIN_OPTIONS) {
  win_data <- image_manifest[image_manifest$win == win, ]
  if (nrow(win_data) == 0) next
  
  # Order known variables first, then anything else (e.g., Ship Track Overview)
  order_known <- na.omit(match(names(want_patterns), win_data$label))
  order_rest  <- setdiff(seq_len(nrow(win_data)), order_known)
  plot_order  <- c(order_known, order_rest)
  
  for (i in plot_order) {
    row <- win_data[i, ]
    
    # Safety: coerce NA to character NA for file paths
    ts_file  <- if (!is.na(row$timeseries_file)) row$timeseries_file else NA_character_
    map_file <- if (!is.na(row$map_file))        row$map_file        else NA_character_
    
    if (!is.na(ts_file) && !is.na(map_file)) {
      # timeseries + map
      cards[[length(cards) + 1]] <- htmltools::tagAppendAttributes(
        tags$div(
          tags$h3(row$label),
          tags$div(
            class = "combined-plot",
            tags$div(
              class = "timeseries-container",
              tags$img(src = file.path("pngs", ts_file))
            ),
            tags$div(
              class = "map-container",
              tags$iframe(src = file.path("pngs", map_file), frameborder = "0", scrolling = "no")
            )
          )
        ),
        class = "card plot",
        `data-win` = row$win,
        `data-x`   = row$x_mode
      )
    } else if (!is.na(map_file) && is.na(ts_file)) {
      # map-only (e.g., Ship Track Overview)
      cards[[length(cards) + 1]] <- htmltools::tagAppendAttributes(
        tags$div(
          tags$h3(row$label),
          tags$div(
            class = "map-container",
            tags$iframe(src = file.path("pngs", map_file), frameborder = "0", scrolling = "no")
          )
        ),
        class = "card plot",
        `data-win` = row$win,
        `data-x`   = row$x_mode
      )
    }
  }
}


page <- tags$html(
  tags$head(tags$meta(charset="utf-8"), tags$title(title), tags$style(HTML(css)), tags$script(HTML(js))),
  tags$body(
    tags$h1(title),
    tags$div(class="meta", " · Report generated ", format(Sys.time(), "on %A, %e %B %Y at %H:%M %Z")," | ", format(Sys.time(), tz="UTC", usetz = TRUE)),
    tags$div(class="meta", HTML(" · Subscribe to calendar updates for Event Log and Upcoming Schedule | <a href='https://calendar.google.com/calendar/embed?src=d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com&ctz=America%2FToronto'>Google Cal</a> | <a href='https://calendar.google.com/calendar/ical/d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com/public/basic.ics'>iCal</a>")),
    tags$div(class="meta", HTML(" · Subscribe to calendar updates on high-surprise events | <a href='https://calendar.google.com/calendar/embed?src=7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com&ctz=America%2FToronto'>Google Cal</a> | <a href='https://calendar.google.com/calendar/ical/7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com/public/basic.ics'>iCal</a>")),
    tags$div(class="meta", HTML(" · For microbial eDNA dashboard go here: <a href='/edna/index.html'>eDNA dashboard</a>")),
    tags$div(class="meta", HTML(" · For auto-refreshing page go here: <a href='dashboard.html'>Underway dashboard</a>")),
    tags$div(cards),
    tags$div(class="meta", " · Daily data files: ", length(files), " · Records: ", nrow(df_all)),
    tags$div(class="meta", HTML(" · Dashboard developed by Eric Collins · Data provided by Amundsen Science (\\\\10.0.0.10\\Data\\FULL_CSV\\2026_LEG_03)"))
  )
)

page_dashboard <-tags$html(
  tags$head(
      tags$meta(charset="utf-8"),
      tags$title(title),
      tags$style(HTML(css)),
      tags$script(HTML(js)),
      tags$meta(`http-equiv` = "refresh", content = "180")
      ),
  tags$body(
    tags$h1(title),
    tags$div(class="meta", " · Report generated ", format(Sys.time(), "on %A,%e %B %Y at %H:%M %Z")," | ", format(Sys.time(), tz="UTC", usetz = TRUE)),
    tags$div(class="meta", HTML(" · Subscribe to calendar updates for Event Log and Upcoming Schedule | <a href='https://calendar.google.com/calendar/embed?src=d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com&ctz=America%2FToronto'>Google Cal</a> | <a href='https://calendar.google.com/calendar/ical/d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d%40group.calendar.google.com/public/basic.ics'>iCal</a>")),
    tags$div(class="meta", HTML(" · Subscribe to calendar updates on high-surprise events | <a href='https://calendar.google.com/calendar/embed?src=7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com&ctz=America%2FToronto'>Google Cal</a> | <a href='https://calendar.google.com/calendar/ical/7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb%40group.calendar.google.com/public/basic.ics'>iCal</a>")),
    tags$div(class="meta", HTML(" · For no-refreshing page go here: <a href='index.html'>Underway dashboard</a>")),
    tags$div(class="meta", HTML(" · For microbial eDNA dashboard go here: <a href='/edna/index.html'>eDNA dashboard</a>")),
    tags$div(cards),
    tags$div(class="meta", " · Daily data files: ", length(files), " · Records: ", nrow(df_all)),
    tags$div(class="meta", HTML(" · Dashboard developed by Eric Collins · Data provided by Amundsen Science (\\\\10.0.0.10\\Data\\FULL_CSV\\2026_LEG_03)"))
  )
)

html_path <- file.path(OUTPUT_DIR, "index.html")
save_html(page, file = html_path)
message("Wrote ", html_path)

html_path_dashboard <- file.path(OUTPUT_DIR, "dashboard.html")
save_html(page_dashboard, file = html_path_dashboard)
message("Wrote ", html_path_dashboard)

# to manually sync
message("To push to webserver do\n",paste0("rsync -av ",OUTPUT_DIR,"/ ",WEB_DIR))
#system("rsync -av $OUTPUT_DIR/ $WEBROOT")



# # Send email
# send_email_alert <- function(subject, message) {
#   email <- compose_email(body = md(message))
  
#   smtp_send(
#     email,
#     to = "rec3141@gmail.com",
#     from = "rec3141@gmail.com", 
#     subject = subject,
#     credentials = creds_file(".gmail_creds")
#   )
# }

# if (surprise_most_recent > 1 & minutes_ago < 20) {
#   email_subject = paste0("[Amundsen Underway] Surprise of ", round(surprise_most_recent,1), format(minute_most_recent_local, " detected at %H:%M on %A,%e %B %Y"))
#   email_body = paste0("Report generated ", format(Sys.time(), "on %A,%e %B %Y at %H:%M %Z"))
#   message(email_subject, "\n", email_body)
#   send_email_alert(email_subject, email_body)
# }

min_surprise = 1
alert_surprise = 2
pick_over_thresh = which(df_min$surprise_score > min_surprise)
minute_over_thresh = df_min$minute[pick_over_thresh]
minute_most_recent = max(minute_over_thresh)
minute_most_recent_local = as.POSIXct(minute_most_recent, tz = LOCAL_TZ)
surprise_most_recent = df_min$surprise_score[df_min$minute==minute_most_recent]
minutes_ago = as.numeric(Sys.time() - minute_most_recent_local, units="mins")

#CAL_ID <- NULL
CAL_ID <- "7ae4b788832de21af8d8aea44eb379098a4f5e1fb2f7dc9262af18c380b62abb@group.calendar.google.com"
SA_JSON <- "~/Desktop/underway_dev/.noble-vortex-471516-d6-7c784035a438.json"
#SA_JSON <- Sys.getenv("GCAL_SERVICE_JSON")
MY_EMAIL <- "cryomics@gmail.com"                   # <-- whoever should see/edit the calendar
TZ      <- "America/Toronto"
ALERT_KEY <- "underway_surprise"   # the stable marker
WINDOW_MIN <- 10                      # duration of the event

# --- auth (service account) ---

tok <- gargle::credentials_service_account(
  scopes = "https://www.googleapis.com/auth/calendar",  # full calendar scope
  path   = SA_JSON
)
bearer <- tok$credentials$access_token

base   <- "https://www.googleapis.com/calendar/v3"
enc    <- function(x) utils::URLencode(x, reserved = TRUE)
rfc3339 <- function(t) format(t, "%Y-%m-%dT%H:%M:%S")

# 1) Create a calendar owned by the service account
## JUST RUN THIS ONCE
if(is.null(CAL_ID)) {
cal <- request(paste0(base, "/calendars")) |>
  req_auth_bearer_token(bearer) |>
  req_body_json(list(summary = "Underway Updates")) |>
  req_perform() |>
  resp_check_status() |>
  resp_body_json()
CAL_ID <- cal$id
cat("Created calendar: ", CAL_ID, "\n")

# 2) Share it to yourself (writer). Repeat this block for anyone else.
request(paste0(base, "/calendars/", enc(CAL_ID), "/acl")) |>
  req_auth_bearer_token(bearer) |>
  req_body_json(list(
    role  = "writer",
    scope = list(type = "user", value = MY_EMAIL)
  )) |>
  req_perform() |>
  resp_check_status()
cat("Shared to ", MY_EMAIL, " with writer access\n")

# Public read access (event details). Use role="freeBusyReader" if you only want free/busy.
request(paste0(base, "/calendars/", enc(CAL_ID), "/acl")) |>
  req_auth_bearer_token(bearer) |>
  req_body_json(list(role = "reader", scope = list(type = "default"))) |>
  req_perform() |>
  resp_check_status() |>
  resp_body_json() #|> print()

} else {
  cat("Using existing calendar: ", CAL_ID, "\n")
}

find_event_id <- function() {
  request(paste0(base, "/calendars/", enc(CAL_ID), "/events")) |>
    req_auth_bearer_token(bearer) |>
    req_url_query(
      privateExtendedProperty = paste0("alertKey=", ALERT_KEY),
      maxResults = 1,
      singleEvents = TRUE,
      orderBy = "updated",
      showDeleted = FALSE
    ) |>
    req_perform() |>
    resp_check_status() |>
    resp_body_json() |>
    (\(x) if (length(x$items) && !is.null(x$items[[1]]$id)) x$items[[1]]$id else NULL)()
}


upsert_alert_event <- function(value) {
  now <- Sys.time()
  nowtxt <- format(now, tz = "America/Toronto", usetz = TRUE)
  body <- list(
    summary = sprintf("Surprise of %g detected at %s", signif(value, 2), nowtxt),
    description = sprintf("Surprise of %g detected at %s", signif(value, 2), nowtxt),
    start = list(dateTime = rfc3339(now + 60), timeZone = TZ),
    end   = list(dateTime = rfc3339(now + WINDOW_MIN*60), timeZone = TZ),
    colorId = if (value > alert_surprise) "11" else "10",
    extendedProperties = list(private = list(alertKey = ALERT_KEY)),
    reminders = list(
      useDefault = FALSE,
      overrides = list(
        list(method = "popup", minutes = 0)    # <- rings at start time
        # you can add more, e.g. list(method="popup", minutes=10)
      )
    )
  )

  id <- find_event_id()
  if (is.null(id)) {
    # First run: create the event (with our marker)
    request(paste0(base, "/calendars/", enc(CAL_ID), "/events")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(body) |>
      req_perform() |>
      resp_check_status() |>
      resp_body_json() #|> print()
  } else {
    # Subsequent runs: PATCH the same event (time + title)
    request(paste0(base, "/calendars/", enc(CAL_ID), "/events/", enc(id))) |>
      req_method("PATCH") |>
      req_auth_bearer_token(bearer) |>
      req_body_json(body) |>
      req_perform() |>
      resp_check_status() |>
      resp_body_json() #|> print()
  }
}

#send calendar update
if (surprise_most_recent > alert_surprise & minutes_ago < 19) {
  upsert_alert_event(surprise_most_recent)
}

