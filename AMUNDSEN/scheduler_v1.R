# ==============================
# Adds grouping + merge to calendar flow
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

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && all(is.na(x)))) y else x

# --------- CONFIG ---------

CAL_ID <- "d8d73fcd6bb2cf89d766d2d9606c40dc1810c5b8c8d7d52d68a2b370efd0aa5d@group.calendar.google.com"
SA_JSON  <- "/home/cryomics/Desktop/Amundsen-Collins/.noble-vortex-471516-d6-7c784035a438.json"
MY_EMAIL <- "cryomics@gmail.com"
TZ       <- "America/Toronto"

# file inputs
EVENTLOG_XLSX <- "/home/cryomics/Desktop/Amundsen-Data/EventLog/2025_LEG_04/Eventlog_2025_LEG_04.xls"
UPCOMING_HTML <- "http://10.0.0.2/Schedule.html"
BASE_HREF      <- NULL                      # e.g., "https://server/path/" to resolve relative links

# EventLog timezone handling
EVENTLOG_SOURCE_TZ <- TZ           # the timezone the EventLog timestamps are MEANT to represent
EVENTLOG_TIME_IS_LOCAL_CLOCK <- TRUE
# TRUE  = treat parsed POSIXct values as local clock time and TAG them with EVENTLOG_SOURCE_TZ (no shifting)
# FALSE = treat parsed POSIXct values as real instants and CONVERT from their stored tz to TZ
# HTML schedule times are local wall-clock (not UTC)
HTML_TIMES_ARE_LOCAL_CLOCK <- TRUE

POLL_SECONDS   <- 1200

COLOR_EVENTLOG <- "2"  # past
COLOR_UPCOMING  <- "5"  # future

base_url <- "https://www.googleapis.com/calendar/v3"
enc      <- function(x) utils::URLencode(x, reserved = TRUE)
# RFC3339 with numeric offset, e.g. "2025-09-09T05:00:00-04:00"
rfc3339 <- function(t, tz = TZ) {
  t <- lubridate::with_tz(t, tz)                # ensure the correct tz on the object
  s <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = tz) # gives -0400
  sub("(\\+|\\-)(\\d{2})(\\d{2})$", "\\1\\2:\\3", s)  # -> -04:00
}

# RFC3339 where input 't' should be treated as LOCAL CLOCK time in 'tz'
rfc3339_local_clock <- function(t, tz = TZ) {
  t <- lubridate::force_tz(t, tz)  # tag as local, do NOT shift the clock time
  s <- format(t, "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  sub("(\\+|\\-)(\\d{2})(\\d{2})$", "\\1\\2:\\3", s)
}

excel_numeric_to_posixct <- function(x, tz) {
  # Excel stores days since 1899-12-30; multiply by seconds per day
  as.POSIXct(as.numeric(x) * 86400, origin = "1899-12-30", tz = tz)
}

bearer_token <- function() {
  tok <- gargle::credentials_service_account(
    scopes = "https://www.googleapis.com/auth/calendar",
    path   = SA_JSON
  )
  tok$credentials$access_token
}

# -------------- Calendar helpers (unchanged) --------------
ensure_calendar <- function(bearer, cal_id = NULL, summary = "Underway Updates", share_with = MY_EMAIL) {
  if (is.null(cal_id)) {
    cal <- request(paste0(base_url, "/calendars")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(summary = summary)) |>
      req_perform() |>
      resp_check_status() |>
      resp_body_json()
    cal_id <- cal$id
    cat("Created calendar:", cal_id, "\n")
    
    request(paste0(base_url, "/calendars/", enc(cal_id), "/acl")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(role = "writer", scope = list(type = "user", value = share_with))) |>
      req_perform() |>
      resp_check_status()
    cat("Shared to", share_with, "as writer\n")
    
    request(paste0(base_url, "/calendars/", enc(cal_id), "/acl")) |>
      req_auth_bearer_token(bearer) |>
      req_body_json(list(role = "reader", scope = list(type = "default"))) |>
      req_perform() |>
      resp_check_status() |> invisible()
  } else {
    cat("Using existing calendar:", cal_id, "\n")
  }
  cal_id
}

set_calendar_timezone <- function(bearer, cal_id, tz = TZ) {
  request(paste0(base_url, "/calendars/", enc(cal_id))) |>
    req_method("PATCH") |>
    req_auth_bearer_token(bearer) |>
    req_body_json(list(timeZone = tz)) |>
    req_perform() |>
    resp_check_status() |> invisible()
}

list_all_event_ids <- function(bearer, cal_id) {
  out <- character(); page <- NULL
  repeat {
    req <- request(paste0(base_url, "/calendars/", enc(cal_id), "/events")) |>
      req_auth_bearer_token(bearer) |>
      req_url_query(singleEvents = TRUE, showDeleted = FALSE, maxResults = 2500)
    if (!is.null(page)) req <- req |> req_url_query(pageToken = page)
    
    res <- req |> req_perform() |> resp_check_status() |> resp_body_json()
    items <- res$items %||% list()
    ids <- if (length(items)) purrr::map_chr(items, "id", .default = NA_character_) else character()
    out <- c(out, ids[!is.na(ids)])
    page <- res$nextPageToken %||% NULL
    if (is.null(page)) break
  }
  out
}

delete_all_events <- function(bearer, cal_id) {
  ids <- list_all_event_ids(bearer, cal_id)
  if (!length(ids)) { cat("No events to delete.\n"); return(invisible()) }
  cat("Deleting", length(ids), "events...\n")
  for (id in ids) {
    request(paste0(base_url, "/calendars/", enc(cal_id), "/events/", enc(id))) |>
      req_method("DELETE") |>
      req_auth_bearer_token(bearer) |>
      req_perform() |>
      resp_check_status()
    Sys.sleep(0.05)
  }
  cat("Deleted all events.\n")
}

post_event <- function(bearer, cal_id, body) {
  request(paste0(base_url, "/calendars/", enc(cal_id), "/events")) |>
    req_auth_bearer_token(bearer) |>
    req_body_json(body) |>
    req_perform() |>
    resp_check_status() |>
    resp_body_json()
}

# -------------- Readers --------------
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
      # keep as character; do NOT call hm()
      start          = .data$start,
      end            = .data$end,
      duration_hour  = suppressWarnings(readr::parse_number(`duration_hour` %||% duration_hour)),
      timing         = "future"
    )
}


# ---- NEW: summarize EventLog Excel into operation/day ranges ----
summarize_eventlog_excel <- function(xlsx_path,
                                      tz = TZ,
                                      source_tz = EVENTLOG_SOURCE_TZ,
                                      source_is_clock_time = EVENTLOG_TIME_IS_LOCAL_CLOCK) {
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
      date       = as.Date(time_local)    # in target tz
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


# -------------- Event building --------------
to_datetime <- function(the_date, tod, tz) {
  if (is.na(the_date) || is.null(tod) || all(is.na(tod))) return(NA_real_)
  
  # Convert time-of-day (tod) to seconds since midnight
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
  # Instead of as.POSIXct(the_date, tz = tz) which interprets as UTC midnight
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
  
  # IMPORTANT: For HTML ("future") rows, treat datetimes as local wall clock.
  is_future <- identical(row$timing, "future")
  fmt <- if (is_future && isTRUE(HTML_TIMES_ARE_LOCAL_CLOCK)) rfc3339_local_clock else rfc3339
  
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



# -------------- Main sync (updated) --------------
sync_once <- function(bearer,
                      cal_id,
                      eventlog_path,
                      upcoming_path,
                      base_href = NULL,
                      drop_canceled = TRUE) {
  
  eventlog_agg <- summarize_eventlog_excel(eventlog_path, tz = TZ)
  
  upcoming_df <- read_upcoming_html(upcoming_path, base_href = base_href) %>%
    clean_names()
  
  if (drop_canceled) {
    eventlog_agg <- eventlog_agg %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
    upcoming_df   <- upcoming_df  %>% filter(is.na(status) | !str_detect(tolower(status), "cancel"))
  }
  
  # Ensure same columns
  needed <- c("station","operations","status","date","start","end","duration_hour","comment","operations_href","timing")
  for (nm in setdiff(needed, names(upcoming_df))) upcoming_df[[nm]] <- NA
  upcoming_df <- upcoming_df %>% mutate(timing = "future") %>% select(all_of(needed))
  eventlog_agg <- eventlog_agg %>% select(all_of(needed))
  
  to_push <- bind_rows(eventlog_agg, upcoming_df)
  
  # 1) wipe
  delete_all_events(bearer, cal_id)
  
  # 2) recreate with color by timing; popup for future only
  n <- 0L
  for (i in seq_len(nrow(to_push))) {
    row  <- to_push[i, ]
    color <- if (identical(row$timing, "future")) COLOR_UPCOMING else COLOR_EVENTLOG
    popup <- if (identical(row$timing, "future")) 0 else NULL
    
    body <- row_to_event_body(row, TZ, color, add_popup_minutes = popup)
    if (!is.null(body)) { post_event(bearer, cal_id, body); n <- n + 1L; Sys.sleep(0.05) }
  }
  cat("Added", n, "events (", sum(to_push$timing == "past"), "past /", sum(to_push$timing == "future"), "future )\n")
  invisible(to_push)
}

# -------------- Loop runner --------------
run_ingestion_loop <- function() {
  bearer <- bearer_token()
  cal_id <- ensure_calendar(bearer, CAL_ID, summary = "Underway Updates", share_with = MY_EMAIL)
  
  set_calendar_timezone(bearer, CAL_ID, TZ)
  
  repeat {
    cat("\n=== Sync cycle at", format(Sys.time(), tz = TZ, usetz = TRUE), "===\n")
    try({
      sync_once(
        bearer,
        cal_id,
        eventlog_path = EVENTLOG_XLSX,
        upcoming_path  = UPCOMING_HTML,
        base_href      = BASE_HREF,
        drop_canceled  = TRUE
      )
    }, silent = FALSE)
    Sys.sleep(POLL_SECONDS)
  }
}

# --- start when ready ---
run_ingestion_loop()
