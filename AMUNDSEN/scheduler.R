# ==============================
# CHECKSUM-BASED Scheduler (Cleaner approach per user suggestion)
# ==============================
suppressPackageStartupMessages({
library(magrittr);library(httr2);library(gargle);library(dplyr);library(purrr);library(readr);library(readxl);library(rvest);library(xml2);library(janitor);library(stringr);library(lubridate);library(tibble);library(digest);library(tidyr);
})

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && all(is.na(x)))) y else x

# --------- CONFIG ---------
output_dir <- "/home/cryomics/Desktop/Amundsen-Collins/"
config <- list(
  calendar = list(
    id = "d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d@group.calendar.google.com",
    timezone = "America/Toronto",
    colors = list(past = "2", future = "5")
  ),
  files = list(
    service_account = file.path("/home/cryomics/Desktop/underway_dev/.noble-vortex-471516-d6-7c784035a438.json"),
    eventlog = "/home/cryomics/Desktop/Amundsen-Data/EventLog/2025_LEG_04/Eventlog_2025_LEG_04.xls",
    upcoming = "http://10.0.0.2/Schedule.html"
  ),
  polling = list(
    interval_seconds = 180,
    api_retry_attempts = 3,
    api_backoff_base = 1,
    api_delay = 0.01
  ),
  owner_email = "cryomics@gmail.com",
  eventlog_settings = list(
    source_tz = "America/Toronto",
    time_is_local_clock = TRUE
  ),
  html_settings = list(
    times_are_local_clock = TRUE,
    base_href = NULL
  ),
  offline = list(
    local_ip <- system("hostname -I | awk '{print $1}'", intern = TRUE),
    ics_file <- file.path(output_dir,"schedule.ics")
  )
)

# --------- SIMPLE CHECKSUM TRACKING ---------
# Store file checksums to detect changes
file_checksums <- new.env()

get_file_checksum <- function(file_path) {
  if (str_detect(file_path, "^https?://")) {
    # For URLs, get content and hash it
    tryCatch({
      content <- read_html(file_path) %>% as.character()
      digest::digest(content, algo = "md5")
    }, error = function(e) {
      # If we can't fetch, return a timestamp-based hash so we try to process
      digest::digest(as.character(Sys.time()), algo = "md5")
    })
  } else {
    # For local files, hash the file contents directly
    if (file.exists(file_path)) {
      digest::digest(file = file_path, algo = "md5")
    } else {
      NA_character_
    }
  }
}

file_has_changed <- function(file_path, key_name) {
  current_checksum <- get_file_checksum(file_path)
  previous_checksum <- file_checksums[[key_name]]
  
  if (is.null(previous_checksum) || is.na(current_checksum) || current_checksum != previous_checksum) {
    # File changed or first run
    file_checksums[[key_name]] <- current_checksum
    return(TRUE)
  }
  
  FALSE  # File unchanged
}

# --------- LOGGING ---------
setup_logging <- function(log_file = file.path(output_dir,"scheduler.log")) {
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

# --------- API HELPERS WITH RETRY ---------
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

# --------- CALENDAR HELPERS (unchanged but with retry) ---------
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

batch_delete_events <- function(bearer, cal_id, event_ids, batch_size = 20) {
  if (!length(event_ids)) return(invisible())
  
  logger$info(paste("Deleting", length(event_ids), "events"))
  batches <- split(event_ids, ceiling(seq_along(event_ids) / batch_size))
  
  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    for (id in batch) {
      request(paste0(base_url, "/calendars/", enc(cal_id), "/events/", enc(id))) |>
        req_method("DELETE") |>
        req_auth_bearer_token(bearer) |>
        api_call_with_retry()
      Sys.sleep(config$polling$api_delay)
    }
  }
}

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

# --------- DATA PROCESSING (unchanged) ---------
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
  
  # Fixed timezone handling
  midnight_local <- as.POSIXct(paste(the_date, "00:00:00"), tz = tz)
  midnight_local + secs
}

fallback_end <- function(start_dt, end_dt, duration_hour) {
  if (!is.na(end_dt) && !is.na(start_dt) && end_dt > start_dt) return(end_dt)
  if (!is.na(start_dt) && !is.na(duration_hour) && duration_hour > 0) {
    return(start_dt + as.difftime(duration_hour, units = "hours"))  # Use difftime, not hours()
  }
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

# --------- CHECKSUM-BASED SYNC ---------
# REPLACE THIS FUNCTION:
sync_with_checksum_detection <- function(bearer, cal_id, drop_canceled = TRUE) {
  
  # Check if either file has changed
  eventlog_changed <- file_has_changed(config$files$eventlog, "eventlog")
  upcoming_changed <- file_has_changed(config$files$upcoming, "upcoming")
  
  if (!eventlog_changed && !upcoming_changed) {
    logger$info("No files changed - skipping processing entirely")
    return(invisible(NULL))
  }
  
  # At least one file changed - do full refresh
  if (eventlog_changed) logger$info("EventLog file changed - full refresh needed")
  if (upcoming_changed) logger$info("HTML schedule changed - full refresh needed")
  
  # Process data
  eventlog_agg <- summarize_eventlog_excel(config$files$eventlog)
  upcoming_df <- read_upcoming_html(config$files$upcoming, config$html_settings$base_href) %>%
    clean_names()
  
  if (drop_canceled) {
    eventlog_agg <- eventlog_agg %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
    upcoming_df  <- upcoming_df %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  }
  
  # Standardize columns
  needed <- c("station","operations","status","date","start","end","duration_hour","comment","operations_href","timing")
  for (nm in setdiff(needed, names(upcoming_df))) upcoming_df[[nm]] <- NA
  upcoming_df <- upcoming_df %>% mutate(timing = "future") %>% select(all_of(needed))
  eventlog_agg <- eventlog_agg %>% select(all_of(needed))
  
  new_events <- bind_rows(eventlog_agg, upcoming_df)
  
  # SIMPLE: Full refresh when files change
  existing_events <- get_existing_events_with_content(bearer, cal_id)
  if (nrow(existing_events) > 0) {
    batch_delete_events(bearer, cal_id, existing_events$event_id)
  }
  
  if (nrow(new_events) > 0) {
    batch_create_events(bearer, cal_id, new_events)
  }
  
  logger$info(paste("Full refresh complete: Deleted", nrow(existing_events), "| Created", nrow(new_events)))
  invisible(new_events)
}

# --------- MAIN LOOP ---------
run_checksum_based_loop <- function() {
  bearer <- bearer_token()
  token_refresh_time <- Sys.time()
  
  cal_id <- ensure_calendar(bearer, config$calendar$id, 
                            summary = "Underway Updates", 
                            share_with = config$owner_email)
  
  set_calendar_timezone(bearer, cal_id, config$calendar$timezone)
  
  # Force initial sync (clear checksums to ensure first run processes)
  logger$info("Performing initial full sync...")
  rm(list = ls(envir = file_checksums), envir = file_checksums)  # Clear checksums
  tryCatch({
    sync_with_checksum_detection(bearer, cal_id, drop_canceled = TRUE)
  }, error = function(e) {
    logger$error(paste("Initial sync failed:", e$message))
  })
  
  repeat {
    # Refresh token if needed
    if (difftime(Sys.time(), token_refresh_time, units = "mins") > 50) {
      bearer <- bearer_token()
      token_refresh_time <- Sys.time()
      logger$info("Bearer token refreshed")
    }
    
    logger$info(paste("=== Checksum-based sync cycle at", 
                      format(Sys.time(), tz = config$calendar$timezone, usetz = TRUE), "==="))
    
    tryCatch({
      sync_with_checksum_detection(bearer, cal_id, drop_canceled = TRUE)
    }, error = function(e) {
      logger$error(paste("Sync cycle failed:", e$message))
    })
    
    Sys.sleep(config$polling$interval_seconds)
  }
}

# ==============================  
# SIMPLIFIED DATA DELTA SYNC
# ==============================

# Store processed data fingerprints  
processed_data_cache <- new.env()

# Create fingerprint of processed event data (after all transformations)
create_processed_data_fingerprint <- function(processed_events) {
  if (nrow(processed_events) == 0) {
    return(digest::digest("empty", algo = "md5"))
  }
  
  # Sort by date/time to ensure consistent ordering
  sorted_events <- processed_events %>%
    arrange(date, start, station, operations)
  
  # Create fingerprint from the key fields that affect calendar events
  key_data <- sorted_events %>%
    select(station, operations, status, date, start, end, timing) %>%
    # Convert to strings for consistent hashing
    mutate(across(everything(), as.character)) %>%
    # Create a single string representation
    unite("row_key", everything(), sep = "|") %>%
    pull(row_key)
  
  # Hash the combined data
  digest::digest(key_data, algo = "md5")
}

# Process raw data into events (your existing logic, consolidated)
process_all_raw_data <- function() {
  
  # Read and process EventLog
  eventlog_events <- tryCatch({
    summarize_eventlog_excel(config$files$eventlog)
  }, error = function(e) {
    logger$warn(paste("EventLog processing failed:", e$message))
    tibble()
  })
  
  # Read and process HTML  
  html_events <- tryCatch({
    upcoming_df <- read_upcoming_html(config$files$upcoming, config$html_settings$base_href) %>%
      clean_names()
    
    # Standardize columns
    needed <- c("station","operations","status","date","start","end","duration_hour","comment","operations_href")
    for (nm in setdiff(needed, names(upcoming_df))) upcoming_df[[nm]] <- NA
    
    upcoming_df %>%
      mutate(timing = "future") %>%
      select(all_of(c(needed, "timing")))
    
  }, error = function(e) {
    logger$warn(paste("HTML processing failed:", e$message))
    tibble()
  })
  
  # Combine and return
  bind_rows(eventlog_events, html_events)
}

# Main sync function with processed data delta detection
sync_with_processed_data_deltas <- function(bearer, cal_id, drop_canceled = TRUE) {

  # First check: Have files changed at all?
  eventlog_changed <- file_has_changed(config$files$eventlog, "eventlog")
  upcoming_changed <- file_has_changed(config$files$upcoming, "upcoming")
  
  if (!eventlog_changed && !upcoming_changed) {
    logger$info("No files changed - skipping processing entirely")
    return(invisible(NULL))
  }
  
  logger$info("Files changed - checking if processed data changed...")
  
  # Process the raw data
  all_processed_events <- process_all_raw_data()
  
  # Apply cancellation filter
  if (drop_canceled && nrow(all_processed_events) > 0) {
    all_processed_events <- all_processed_events %>%
      filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  }
  
  # Create fingerprint of processed data
  current_fingerprint <- create_processed_data_fingerprint(all_processed_events)
  previous_fingerprint <- processed_data_cache$last_processed_fingerprint %||% ""
  
  if (current_fingerprint == previous_fingerprint) {
    logger$info("Files changed but processed data identical - no calendar updates needed")
    return(invisible(NULL))
  }
  
  logger$info("Processed data changed - performing incremental calendar sync")
  
  # Store current fingerprint
  processed_data_cache$last_processed_fingerprint <- current_fingerprint
  
  # Now do incremental calendar sync (your working logic)
  new_events <- all_processed_events
  
  # Create fingerprints for new events (for calendar comparison)
  new_events_with_fp <- new_events %>%
    rowwise() %>%
    mutate(
      # Create calendar event fingerprint
      calendar_fingerprint = {
        station <- station %||% ""
        ops     <- operations %||% ""
        status  <- status %||% ""
        
        summary_text <- paste0(
          if (nzchar(status)) sprintf("[%s] ", status) else "",
          if (nzchar(station)) sprintf("%s — ", station) else "",
          ops
        ) %>% str_squish()
        
        color_id <- if (identical(timing, "future")) config$calendar$colors$future else config$calendar$colors$past
        
        # Create consistent fingerprint
        digest::digest(list(
          as.character(date),
          start %||% "",
          end %||% "",
          summary_text,
          color_id
        ), algo = "md5")
      }
    ) %>%
    ungroup()
  
  # Get existing events from calendar
  existing_events <- get_existing_events_with_content(bearer, cal_id)

  if (nrow(existing_events) > 0) {
    existing_events_with_fp <- existing_events %>%
      rowwise() %>%
      mutate(
        calendar_fingerprint = {
          # Extract date from start_dt
          start_dt_parsed <- tryCatch({
            lubridate::ymd_hms(start_dt, tz = config$calendar$timezone, quiet=TRUE)
          }, error = function(e) NA)
          
          event_date <- if (!is.na(start_dt_parsed)) as.character(as.Date(start_dt_parsed)) else ""
          start_time <- if (!is.na(start_dt_parsed)) format(start_dt_parsed, "%H:%M") else ""
          
          end_dt_parsed <- tryCatch({
            lubridate::ymd_hms(end_dt, tz = config$calendar$timezone, quiet=TRUE)  
          }, error = function(e) NA)
          end_time <- if (!is.na(end_dt_parsed)) format(end_dt_parsed, "%H:%M") else ""
          
          digest::digest(list(
            event_date,
            start_time,
            end_time,
            str_squish(summary %||% ""),
            color_id %||% ""
          ), algo = "md5")
        }
      ) %>%
      ungroup()
  } else {
    existing_events_with_fp <- tibble(event_id = character(), calendar_fingerprint = character())
  }
  
  # Find differences
  to_add <- anti_join(new_events_with_fp, existing_events_with_fp, by = "calendar_fingerprint")
  to_delete <- anti_join(existing_events_with_fp, new_events_with_fp, by = "calendar_fingerprint")
  
  # Apply changes
  if (nrow(to_delete) > 0) {
    logger$info(paste("Deleting", nrow(to_delete), "calendar events"))
    batch_delete_events(bearer, cal_id, to_delete$event_id)
  }
  
  if (nrow(to_add) > 0) {
    logger$info(paste("Creating", nrow(to_add), "calendar events"))
    batch_create_events(bearer, cal_id, to_add)
  }
  
  unchanged_count <- nrow(new_events) - nrow(to_add)
  logger$info(paste("Incremental sync complete: Added", nrow(to_add), "| Deleted", nrow(to_delete), 
                    "| Unchanged", unchanged_count))
  

  invisible(new_events)
}

# ================================================================
# FALLBACK: Simple approach if the above has issues
# ================================================================

sync_with_simple_data_delta <- function(bearer, cal_id, drop_canceled = TRUE) {
  
  # Check files first
  eventlog_changed <- file_has_changed(config$files$eventlog, "eventlog")
  upcoming_changed <- file_has_changed(config$files$upcoming, "upcoming")
  
  if (!eventlog_changed && !upcoming_changed) {
    logger$info("No files changed - skipping processing entirely")
    return(invisible(NULL))
  }
  
  # Process data
  all_processed_events <- process_all_raw_data()
  if (drop_canceled && nrow(all_processed_events) > 0) {
    all_processed_events <- all_processed_events %>%
      filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  }
  
  # Simple approach: check if processed data changed
  current_fp <- create_processed_data_fingerprint(all_processed_events)
  previous_fp <- processed_data_cache$last_simple_fingerprint %||% ""
  
  if (current_fp == previous_fp) {
    logger$info("Processed data unchanged - no calendar updates needed")
    return(invisible(NULL))
  }
  
  # Data changed - do full calendar refresh (fast and reliable)
  logger$info("Processed data changed - full calendar refresh")
  processed_data_cache$last_simple_fingerprint <- current_fp
  
  existing_events <- get_existing_events_with_content(bearer, cal_id)
  if (nrow(existing_events) > 0) {
    batch_delete_events(bearer, cal_id, existing_events$event_id)
  }
  
  if (nrow(all_processed_events) > 0) {
    batch_create_events(bearer, cal_id, all_processed_events)
  }
  
  logger$info(paste("Full refresh: Deleted", nrow(existing_events), "| Created", nrow(all_processed_events)))
  invisible(all_processed_events)
}


# ==============================
# OFFLINE CALENDAR: ICS File Generation
# ==============================

# Generate ICS file from your processed events
generate_ics_file <- function(events_df, output_path = "schedule.ics") {
  
  if (nrow(events_df) == 0) {
    logger$warn("No events to export to ICS")
    return(invisible())
  }
  
  # ICS header
  ics_lines <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0", 
    "PRODID:-//Amundsen Expedition//Scheduler//EN",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    paste0("X-WR-CALNAME:", "Amundsen Underway Operations"),
    paste0("X-WR-TIMEZONE:", config$calendar$timezone),
    ""
  )
  
  # Process each event
  for (i in seq_len(nrow(events_df))) {
    row <- events_df[i, ]
    
    # Create start/end datetimes
    start_dt <- to_datetime(row$date %||% NA, row$start %||% NA, config$calendar$timezone)
    end_dt   <- to_datetime(row$date %||% NA, row$end   %||% NA, config$calendar$timezone)
    end_dt   <- fallback_end(start_dt, end_dt, row$duration_hour %||% NA_real_)
    
    if (is.na(start_dt) || is.na(end_dt)) next
    
    # Format for ICS (UTC time in YYYYMMDDTHHMMSSZ format)
    format_ics_datetime <- function(dt) {
      utc_dt <- lubridate::with_tz(dt, "UTC")
      format(utc_dt, "%Y%m%dT%H%M%SZ")
    }
    
    # Create event summary and description
    station <- row$station %||% ""
    ops     <- row$operations %||% ""
    status  <- row$status %||% ""
    comment <- row$comment %||% ""
    
    summary <- paste0(
      if (nzchar(status)) sprintf("[%s] ", status) else "",
      if (nzchar(station)) sprintf("%s — ", station) else "",
      ops
    ) %>% stringr::str_squish()
    
    description <- paste(
      c(
        if (nzchar(ops))     paste0("Operation: ", ops),
        if (nzchar(station)) paste0("Station: ", station),
        if (nzchar(status))  paste0("Status: ", status),
        if (nzchar(comment)) paste0("Comment: ", comment)
      ),
      collapse = "\\n"  # ICS uses \\n for line breaks
    )
    
    # Generate unique ID
    uid <- paste0(digest::digest(paste(summary, start_dt, end_dt), algo = "md5"), "@amundsen.expedition")
    
    # Create timestamp (when this event was created/modified)
    dtstamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    
    # Event lines
    event_lines <- c(
      "BEGIN:VEVENT",
      paste0("UID:", uid),
      paste0("DTSTAMP:", dtstamp),
      paste0("DTSTART:", format_ics_datetime(start_dt)),
      paste0("DTEND:", format_ics_datetime(end_dt)),
      paste0("SUMMARY:", summary),
      paste0("DESCRIPTION:", description),
      paste0("CATEGORIES:", if (identical(row$timing, "future")) "Scheduled" else "EventLog"),
      paste0("STATUS:", if (identical(row$timing, "future")) "TENTATIVE" else "CONFIRMED"),
      "END:VEVENT",
      ""
    )
    
    ics_lines <- c(ics_lines, event_lines)
  }
  
  # ICS footer
  ics_lines <- c(ics_lines, "END:VCALENDAR")
  
  # Write to file
  writeLines(ics_lines, output_path, useBytes = TRUE)
  logger$info(paste("Generated ICS file with", nrow(events_df), "events at", output_path))
  
  invisible(output_path)
}

# ================================================================
# LOCAL WEB SERVER SETUP OPTIONS
# ================================================================

# OPTION 1: Use existing web server (if you have Apache/Nginx)
# Just point the output to your web directory:
output_path = config$offline$ice_file
# Then access via: http://your-ship-ip/schedule.ics


# ================================================================
# MOBILE DEVICE SETUP INSTRUCTIONS
# ================================================================

mobile_setup_instructions <- function(ship_ip = "10.0.0.2", port = 8080) {
  cat("=== MOBILE DEVICE SETUP ===\n\n")
  
  cat("📱 iPhone/iPad:\n")
  cat("1. Open Settings → Calendar → Accounts\n")
  cat("2. Add Account → Other → Add Subscribed Calendar\n")
  cat("3. Enter URL: http://", ship_ip, ":", port, "/schedule.ics\n")
  cat("4. Name it 'Amundsen Operations'\n")
  cat("5. Set refresh frequency (15 minutes recommended)\n\n")
  
  cat("🤖 Android:\n")
  cat("1. Open Google Calendar app\n")
  cat("2. Menu → Settings → Add calendar → From URL\n")
  cat("3. Enter URL: http://", ship_ip, ":", port, "/schedule.ics\n")
  cat("4. Or use a third-party app like ICSx5 for better local calendar support\n\n")
  
  cat("🌐 Any device with web browser:\n")
  cat("1. Go to: http://", ship_ip, ":", port, "/\n")
  cat("2. Download the ICS file manually\n")
  cat("3. Import into your calendar app\n\n")
  
  cat("🔄 The calendar will auto-refresh every 15-30 minutes\n")
  cat("📶 Works entirely over local network - no internet required!\n")
}

# ================================================================
# MODIFIED MAIN SYNC FUNCTION FOR ICS OUTPUT
# ================================================================

sync_with_ics_output <- function(bearer = NULL, cal_id = NULL, ics_output_path = ics_file, drop_canceled = NULL) {
  
  # Check if files changed
  eventlog_changed <- file_has_changed(config$files$eventlog, "eventlog")
  upcoming_changed <- file_has_changed(config$files$upcoming, "upcoming")
  
  if (!eventlog_changed && !upcoming_changed) {
    logger$info("No files changed - skipping processing entirely")
    return(invisible(NULL))
  }
  
  logger$info("Files changed - updating ICS file")
  
  # Process data (same as before)
  all_processed_events <- process_all_raw_data()
  
  # Filter canceled events
  all_processed_events <- all_processed_events %>%
    filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  
  # Generate ICS file
  generate_ics_file(all_processed_events, ics_output_path)
  
  # OPTIONAL: Also sync to Google Calendar if internet available
  if (!is.null(bearer) && !is.null(cal_id)) {
    tryCatch({
      # Your existing Google Calendar sync logic here
      logger$info("Internet available - also syncing to Google Calendar")
      sync_with_processed_data_deltas(bearer, cal_id, drop_canceled = TRUE)
    }, error = function(e) {
      logger$warn("Google Calendar sync failed (offline?), but ICS file updated")
    })
  }
  
  invisible(all_processed_events)
}

# ================================================================
# COMPLETE OFFLINE CALENDAR SETUP
# ================================================================

setup_offline_calendar <- function(ics_file = "/tmp/schedule.ics", port = 8080) {
  
  cat("🚢 Setting up offline calendar system...\n\n")
  
  # Start the ICS server
  server <- serve_ics_with_r(config$offline$ics_file, port)
  
  # Print setup instructions
  mobile_setup_instructions(port = port)
  
  cat("✅ Offline calendar server is running!\n")
  cat("📁 ICS file location:", config$offline$ics_file, "\n")
  cat("🔗 Subscribe URL: http://",config$offline$local_ip,":", port, "/schedule.ics\n")
  
  return(server)
}


#overwrite function to choose a different one
#sync_with_processed_data_deltas
#sync_with_checksum_detection <- sync_with_ics_output
sync_with_checksum_detection <- sync_with_processed_data_deltas
run_checksum_based_loop()






