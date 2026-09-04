# ==============================
# OPTIMIZED Scheduler with Incremental Updates + Caching + Retry Logic
# ==============================
library(magrittr)
library(httr2)
library(gargle)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(rvest)
library(xml2)
library(janitor)
library(stringr)
library(lubridate)
library(tibble)
library(digest)  # for fingerprinting

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && all(is.na(x)))) y else x

# --------- OPTIMIZED CONFIG ---------
config <- list(
  calendar = list(
    id = "d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d@group.calendar.google.com",
    timezone = "America/Toronto",
    colors = list(past = "2", future = "5")
  ),
  files = list(
    service_account = "/home/cryomics/Desktop/Amundsen-Collins/.noble-vortex-471516-d6-7c784035a438.json",
    eventlog = "/home/cryomics/Desktop/Amundsen-Data/EventLog/2025_LEG_04/Eventlog_2025_LEG_04.xls",
    upcoming = "http://10.0.0.2/Schedule.html"
  ),
  polling = list(
    interval_seconds = 300,
    api_retry_attempts = 3,
    api_backoff_base = 1,
    api_delay = 0.01  # Reduced from 0.05
  ),
  cache = list(
    duration_seconds = 600  # 5 minute cache
  ),
  owner_email = "cryomics@gmail.com",
  eventlog_settings = list(
    source_tz = "America/Toronto",
    time_is_local_clock = TRUE
  ),
  html_settings = list(
    times_are_local_clock = TRUE,
    base_href = NULL
  )
)

# --------- LOGGING ---------
setup_logging <- function(log_file = "scheduler.log") {
  log_message <- function(level, message) {
    timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
    entry <- paste(timestamp, toupper(level), message, sep = " ")
    cat(entry, "\n")
    cat(entry, "\n", file = log_file, append = TRUE)
  }
  
  list(
    info  = function(msg) log_message("info", msg),
    warn  = function(msg) log_message("warn", msg),
    error = function(msg) log_message("error", msg)
  )
}

logger <- setup_logging()

# --------- CACHING SYSTEM ---------
file_cache <- new.env()

get_file_modification_time <- function(path) {
  if (str_detect(path, "^https?://")) {
    # For URLs, we'll cache for the specified duration
    return(Sys.time())
  }
  if (file.exists(path)) file.mtime(path) else as.POSIXct(NA)
}

is_cache_valid <- function(cache_entry, file_path, cache_duration) {
  if (is.null(cache_entry)) return(FALSE)
  
  time_valid <- difftime(Sys.time(), cache_entry$timestamp, units = "secs") < cache_duration
  
  # For local files, also check modification time
  if (!str_detect(file_path, "^https?://")) {
    file_mtime <- get_file_modification_time(file_path)
    if (!is.na(file_mtime) && !is.na(cache_entry$file_mtime)) {
      file_unchanged <- file_mtime <= cache_entry$file_mtime
      return(time_valid && file_unchanged)
    }
  }
  
  time_valid
}

# --------- API HELPERS WITH RETRY LOGIC ---------
base_url <- "https://www.googleapis.com/calendar/v3"
enc <- function(x) utils::URLencode(x, reserved = TRUE)

api_call_with_retry <- function(req, max_attempts = config$polling$api_retry_attempts, 
                                backoff_base = config$polling$api_backoff_base) {
  for (attempt in seq_len(max_attempts)) {
    tryCatch({
      return(req |> req_perform() |> resp_check_status())
    }, error = function(e) {
      if (attempt == max_attempts) {
        logger$error(paste("API call failed after", max_attempts, "attempts:", e$message))
        stop(e)
      }
      
      wait_time <- backoff_base * (2 ^ (attempt - 1))
      logger$warn(paste("API call failed (attempt", attempt, "/", max_attempts, 
                        "), retrying in", wait_time, "seconds..."))
      Sys.sleep(wait_time)
    })
  }
}

# RFC3339 formatters (unchanged)
rfc3339 <- function(t, tz = config$calendar$timezone) {
  t <- lubridate::with_tz(t, tz)
  s <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  sub("(\\+|\\-)(\\d{2})(\\d{2})$", "\\1\\2:\\3", s)
}

rfc3339_local_clock <- function(t, tz = config$calendar$timezone) {
  t <- lubridate::force_tz(t, tz)
  s <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  sub("(\\+|\\-)(\\d{2})(\\d{2})$", "\\1\\2:\\3", s)
}

excel_numeric_to_posixct <- function(x, tz) {
  as.POSIXct(as.numeric(x) * 86400, origin = "1899-12-30", tz = tz)
}

bearer_token <- function() {
  tok <- gargle::credentials_service_account(
    scopes = "https://www.googleapis.com/auth/calendar",
    path   = config$files$service_account
  )
  tok$credentials$access_token
}

# --------- OPTIMIZED CALENDAR HELPERS ---------
ensure_calendar <- function(bearer, cal_id = NULL, summary = "Underway Updates", share_with = config$owner_email) {
  if (is.null(cal_id)) {
    logger$info("Creating new calendar")
    cal <- request(paste0(base_url, "/calendars")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(summary = summary)) |>
      api_call_with_retry() |>
      resp_body_json()
    cal_id <- cal$id
    logger$info(paste("Created calendar:", cal_id))
    
    request(paste0(base_url, "/calendars/", enc(cal_id), "/acl")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(role = "writer", scope = list(type = "user", value = share_with))) |>
      api_call_with_retry()
    logger$info(paste("Shared to", share_with, "as writer"))
    
    request(paste0(base_url, "/calendars/", enc(cal_id), "/acl")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(role = "reader", scope = list(type = "default"))) |>
      api_call_with_retry() |> invisible()
  } else {
    logger$info(paste("Using existing calendar:", cal_id))
  }
  cal_id
}

set_calendar_timezone <- function(bearer, cal_id, tz = config$calendar$timezone) {
  request(paste0(base_url, "/calendars/", enc(cal_id))) |>
    req_method("PATCH") |>
    req_auth_bearer_token(bearer) |>
    req_body_json(list(timeZone = tz)) |>
    api_call_with_retry() |> invisible()
}

# OPTIMIZED: Get existing events with content for comparison
get_existing_events_with_content <- function(bearer, cal_id) {
  events <- list()
  page <- NULL
  
  repeat {
    req <- request(paste0(base_url, "/calendars/", enc(cal_id), "/events")) |>
      req_auth_bearer_token(bearer) |>
      req_url_query(singleEvents = TRUE, showDeleted = FALSE, maxResults = 2500)
    if (!is.null(page)) req <- req |> req_url_query(pageToken = page)
    
    res <- req |> api_call_with_retry() |> resp_body_json()
    items <- res$items %||% list()
    events <- c(events, items)
    
    page <- res$nextPageToken %||% NULL
    if (is.null(page)) break
  }
  
  # Extract relevant fields for comparison
  purrr::map_dfr(events, function(evt) {
    tibble(
      event_id = evt$id %||% NA_character_,
      summary = evt$summary %||% NA_character_,
      description = evt$description %||% NA_character_,
      start_dt = evt$start$dateTime %||% NA_character_,
      end_dt = evt$end$dateTime %||% NA_character_,
      color_id = evt$colorId %||% NA_character_
    )
  })
}

# OPTIMIZED: Batch delete (faster than individual deletes)
batch_delete_events <- function(bearer, cal_id, event_ids, batch_size = 20) {
  if (!length(event_ids)) return(invisible())
  
  logger$info(paste("Deleting", length(event_ids), "events"))
  batches <- split(event_ids, ceiling(seq_along(event_ids) / batch_size))
  
  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    logger$info(paste("Deleting batch", i, "/", length(batches), "(", length(batch), "events)"))
    
    for (id in batch) {
      request(paste0(base_url, "/calendars/", enc(cal_id), "/events/", enc(id))) |>
        req_method("DELETE") |>
        req_auth_bearer_token(bearer) |>
        api_call_with_retry()
      Sys.sleep(config$polling$api_delay)
    }
  }
}

# OPTIMIZED: Batch create
batch_create_events <- function(bearer, cal_id, events_df) {
  if (!nrow(events_df)) return(invisible())
  
  logger$info(paste("Creating", nrow(events_df), "events"))
  
  for (i in seq_len(nrow(events_df))) {
    row <- events_df[i, ]
    color <- if (identical(row$timing, "future")) config$calendar$colors$future else config$calendar$colors$past
    popup <- if (identical(row$timing, "future")) 0 else NULL
    
    body <- row_to_event_body(row, config$calendar$timezone, color, add_popup_minutes = popup)
    if (!is.null(body)) {
      request(paste0(base_url, "/calendars/", enc(cal_id), "/events")) |>
        req_auth_bearer_token(bearer) |>
        req_body_json(body) |>
        api_call_with_retry()
      Sys.sleep(config$polling$api_delay)
    }
  }
}

# --------- CACHED DATA READERS ---------
read_upcoming_html_cached <- function(html_path, base_href = config$html_settings$base_href) {
  cache_key <- paste0("html_", html_path)
  cached <- file_cache[[cache_key]]
  
  if (is_cache_valid(cached, html_path, config$cache$duration_seconds)) {
    logger$info("Using cached HTML data")
    return(cached$data)
  }
  
  logger$info("Reading fresh HTML data")
  data <- read_upcoming_html(html_path, base_href)
  
  file_cache[[cache_key]] <- list(
    data = data,
    timestamp = Sys.time(),
    file_mtime = get_file_modification_time(html_path)
  )
  data
}

summarize_eventlog_excel_cached <- function(xlsx_path) {
  cache_key <- paste0("excel_", xlsx_path)
  cached <- file_cache[[cache_key]]
  
  if (is_cache_valid(cached, xlsx_path, config$cache$duration_seconds)) {
    logger$info("Using cached Excel data")
    return(cached$data)
  }
  
  logger$info("Reading fresh Excel data")
  data <- summarize_eventlog_excel(
    xlsx_path,
    tz = config$calendar$timezone,
    source_tz = config$eventlog_settings$source_tz,
    source_is_clock_time = config$eventlog_settings$time_is_local_clock
  )
  
  file_cache[[cache_key]] <- list(
    data = data,
    timestamp = Sys.time(),
    file_mtime = get_file_modification_time(xlsx_path)
  )
  data
}

# --------- ORIGINAL FUNCTIONS (with optimizations) ---------
read_upcoming_html <- function(html_path, base_href = NULL) {
  doc <- read_html(html_path)
  tbl <- doc %>% html_element("table.center")
  
  trs  <- tbl %>% html_elements("tr")
  hdr  <- trs[[1]] %>% html_elements("th,td") %>% html_text2() %>% str_squish()
  body_trs <- trs %>% .[-1]
  
  rows <- body_trs %>%
    purrr::map(~ .x %>% html_elements(xpath = ".//th|.//td") %>% html_text2() %>% str_squish())
  
  n <- length(hdr)
  mat <- rows %>%
    purrr::map(~ { v <- .x; length(v) <- n; v }) %>%
    do.call(rbind, .) %>%
    as.data.frame(stringsAsFactors = FALSE)
  names(mat) <- janitor::make_clean_names(hdr)
  
  ops_href <- body_trs %>%
    purrr::map_chr(~{
      a <- .x %>% html_element(xpath = ".//a[@href]")
      if (length(a) == 0 || is.na(a)) NA_character_ else a %>% html_attr("href")
    }) %>%
    { if (!is.null(base_href)) xml2::url_absolute(., base_href) else . }
  
  mat %>%
    tibble::as_tibble() %>%
    mutate(
      across(everything(), ~na_if(.x, "")),
      operations_href = ops_href,
      date           = suppressWarnings(lubridate::dmy(.data$date)),
      start          = .data$start,
      end            = .data$end,
      duration_hour  = suppressWarnings(readr::parse_number(`duration_hour` %||% duration_hour)),
      timing         = "future"
    )
}

summarize_eventlog_excel <- function(xlsx_path,
                                     tz = config$calendar$timezone,
                                     source_tz = config$eventlog_settings$source_tz,
                                     source_is_clock_time = config$eventlog_settings$time_is_local_clock) {
  raw <- readxl::read_excel(xlsx_path, guess_max = 10000) %>%
    janitor::clean_names()
  
  tl <- raw$time_local
  
  time_src <- if (inherits(tl, "POSIXct")) {
    if (isTRUE(source_is_clock_time)) {
      lubridate::force_tz(tl, tzone = source_tz)
    } else {
      lubridate::with_tz(tl, tzone = source_tz)
    }
  } else if (is.numeric(tl)) {
    excel_numeric_to_posixct(tl, tz = source_tz)
  } else {
    tmp <- suppressWarnings(lubridate::ymd_hms(tl, tz = source_tz, quiet = TRUE))
    bad <- is.na(tmp)
    if (any(bad)) tmp[bad] <- suppressWarnings(lubridate::ymd_hm(tl[bad], tz = source_tz, quiet = TRUE))
    tmp
  }
  
  time_local <- lubridate::with_tz(time_src, tz = tz)
  
  safe_min <- function(x) { x <- x[!is.na(x)]; if (length(x)) min(x) else as.POSIXct(NA_real_, tz = tz) }
  safe_max <- function(x) { x <- x[!is.na(x)]; if (length(x)) max(x) else as.POSIXct(NA_real_, tz = tz) }
  first_non_na <- function(x) { y <- x[!is.na(x) & nzchar(as.character(x))]; if (length(y)) y[[1]] else NA_character_ }
  
  raw %>%
    mutate(
      time_local = time_local,
      date       = as.Date(time_local)
    ) %>%
    group_by(station_id, activity, station_type, date) %>%
    summarise(
      start_dt = safe_min(time_local),
      end_dt   = safe_max(time_local),
      comment  = first_non_na(comment),
      .groups  = "drop"
    ) %>%
    mutate(
      duration_hour   = as.numeric(difftime(end_dt, start_dt, units = "hours")),
      station         = station_id,
      status          = "EventLog",
      operations      = paste(dplyr::coalesce(activity, ""), dplyr::coalesce(station_type, ""), sep = " | "),
      start           = ifelse(!is.na(start_dt), format(lubridate::with_tz(start_dt, tz), "%H:%M"), NA_character_),
      end             = ifelse(!is.na(end_dt),   format(lubridate::with_tz(end_dt,   tz), "%H:%M"), NA_character_),
      timing          = "past",
      operations_href = NA_character_
    ) %>%
    dplyr::transmute(
      station, operations, status, date, start, end,
      duration_hour, comment, operations_href, timing
    )
}

# FIXED: to_datetime function (your timezone fix)
to_datetime <- function(the_date, tod, tz) {
  if (is.na(the_date) || is.null(tod) || all(is.na(tod))) return(NA_real_)
  
  to_seconds <- function(x) {
    if (lubridate::is.period(x)) return(lubridate::period_to_seconds(x))
    if (inherits(x, "difftime")) return(as.numeric(x, units = "secs"))
    if (inherits(x, "hms"))      return(as.numeric(x))
    if (is.numeric(x))           return(x)
    if (is.character(x)) {
      p <- suppressWarnings(lubridate::hm(x))
      if (is.na(p)) p <- suppressWarnings(lubridate::hms(x))
      if (is.na(p)) return(NA_real_)
      return(lubridate::period_to_seconds(p))
    }
    NA_real_
  }
  
  secs <- to_seconds(tod)
  if (is.na(secs)) return(NA_real_)
  
  # FIX: Properly create POSIXct at midnight in the specified timezone
  midnight_local <- as.POSIXct(paste(the_date, "00:00:00"), tz = tz)
  midnight_local + secs
}

fallback_end <- function(start_dt, end_dt, duration_hour) {
  if (!is.na(end_dt) && !is.na(start_dt) && end_dt > start_dt) return(end_dt)
  if (!is.na(start_dt) && !is.na(duration_hour) && duration_hour > 0) return(start_dt + hours(duration_hour))
  if (!is.na(start_dt)) return(start_dt + minutes(1))
  NA_real_
}

row_to_event_body <- function(row, tz, color_id, add_popup_minutes = NULL) {
  start_dt <- to_datetime(row$date %||% NA, row$start %||% NA, tz)
  end_dt   <- to_datetime(row$date %||% NA, row$end   %||% NA, tz)
  end_dt   <- fallback_end(start_dt, end_dt, row$duration_hour %||% NA_real_)
  if (is.na(start_dt) || is.na(end_dt)) return(NULL)
  if (is.na(row$status)) row$status <- "Scheduled"
  
  station <- row$station %||% ""
  ops     <- row$operations %||% ""
  status  <- row$status %||% ""
  comment <- row$comment %||% ""
  href    <- row$operations_href %||% NA_character_
  
  summary <- paste0(
    if (nzchar(status)) sprintf("[%s] ", status) else "",
    if (nzchar(station)) sprintf("%s — ", station) else "",
    ops
  ) %>% stringr::str_squish()
  
  desc <- paste(
    c(
      if (nzchar(ops))     paste0("Operation: ", ops),
      if (nzchar(station)) paste0("Station: ", station),
      if (nzchar(status))  paste0("Status: ", status),
      if (nzchar(comment)) paste0("Comment: ", comment)
    ),
    collapse = "\n"
  )
  
  is_future <- identical(row$timing, "future")
  fmt <- if (is_future && isTRUE(config$html_settings$times_are_local_clock)) rfc3339_local_clock else rfc3339
  
  body <- list(
    summary = summary,
    description = desc,
    start = list(dateTime = fmt(start_dt, tz), timeZone = tz),
    end   = list(dateTime = fmt(end_dt,   tz), timeZone = tz),
    colorId = color_id,
    reminders = list(useDefault = FALSE)
  )
  if (!is.null(add_popup_minutes)) {
    body$reminders$overrides <- list(list(method = "popup", minutes = add_popup_minutes))
  }
  body
}

# --------- OPTIMIZED MAIN SYNC (INCREMENTAL!) ---------
sync_incremental <- function(bearer, cal_id, drop_canceled = TRUE) {
  # Get new data using cache
  eventlog_agg <- summarize_eventlog_excel_cached(config$files$eventlog)
  upcoming_df <- read_upcoming_html_cached(config$files$upcoming) %>% clean_names()
  
  if (drop_canceled) {
    eventlog_agg <- eventlog_agg %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
    upcoming_df  <- upcoming_df %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  }
  
  # Standardize columns
  needed <- c("station","operations","status","date","start","end","duration_hour","comment","operations_href","timing")
  for (nm in setdiff(needed, names(upcoming_df))) upcoming_df[[nm]] <- NA
  upcoming_df <- upcoming_df %>% mutate(timing = "future") %>% select(all_of(needed))
  eventlog_agg <- eventlog_agg %>% select(all_of(needed))
  
  new_events <- bind_rows(eventlog_agg, upcoming_df) %>%
    # Create fingerprint for comparison
    rowwise() %>%
    mutate(fingerprint = digest::digest(list(station, operations, date, start, end, status))) %>%
    ungroup()
  
  # Get existing events
  existing_events <- get_existing_events_with_content(bearer, cal_id)
  
  # Create fingerprints for existing events (extract from summary/description)
  if (nrow(existing_events) > 0) {
    # This is a simplified fingerprint comparison - in production you might want more sophisticated parsing
    existing_events <- existing_events %>%
      rowwise() %>%
      mutate(fingerprint = digest::digest(list(summary, description, start_dt, end_dt))) %>%
      ungroup()
  } else {
    existing_events <- tibble(event_id = character(), fingerprint = character())
  }
  
  # Find differences
  to_add <- anti_join(new_events, existing_events, by = "fingerprint")
  to_delete <- anti_join(existing_events, new_events, by = "fingerprint")
  
  # Apply changes
  if (nrow(to_delete) > 0) {
    batch_delete_events(bearer, cal_id, to_delete$event_id)
  }
  
  if (nrow(to_add) > 0) {
    batch_create_events(bearer, cal_id, to_add)
  }
  
  logger$info(paste("Sync complete: Added", nrow(to_add), "| Deleted", nrow(to_delete), 
                    "| Unchanged", nrow(new_events) - nrow(to_add)))
  
  invisible(new_events)
}

# --------- OPTIMIZED MAIN LOOP ---------
run_optimized_ingestion_loop <- function() {
  bearer <- bearer_token()
  token_refresh_time <- Sys.time()
  
  cal_id <- ensure_calendar(bearer, config$calendar$id, 
                            summary = "Underway Updates", 
                            share_with = config$owner_email)
  
  set_calendar_timezone(bearer, cal_id, config$calendar$timezone)
  
  # Initial full sync (fallback to old method for first run)
  logger$info("Performing initial full sync...")
  tryCatch({
    # For the very first run, do a full wipe to ensure clean slate
    existing <- get_existing_events_with_content(bearer, cal_id)
    if (nrow(existing) > 0) {
      batch_delete_events(bearer, cal_id, existing$event_id)
    }
    sync_incremental(bearer, cal_id)
  }, error = function(e) {
    logger$error(paste("Initial sync failed:", e$message))
  })
  
  repeat {
    # Refresh token if needed (every 50 minutes)
    if (difftime(Sys.time(), token_refresh_time, units = "mins") > 50) {
      bearer <- bearer_token()
      token_refresh_time <- Sys.time()
      logger$info("Bearer token refreshed")
    }
    
    logger$info(paste("=== Incremental sync cycle at", format(Sys.time(), tz = config$calendar$timezone, usetz = TRUE), "==="))
    
    tryCatch({
      sync_incremental(bearer, cal_id, drop_canceled = TRUE)
    }, error = function(e) {
      logger$error(paste("Sync cycle failed:", e$message))
    })
    
    Sys.sleep(config$polling$interval_seconds)
  }
}

# --------- START OPTIMIZED LOOP ---------
logger$info("Starting optimized scheduler with incremental updates, caching, and retry logic...")
run_optimized_ingestion_loop()