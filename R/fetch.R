month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
day_abbr <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

parse_http_date <- function(x) {
  pattern <- "^[A-Za-z]{3}, ([0-9]{2}) ([A-Za-z]{3}) ([0-9]{4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT$"
  secs <- rep(NA_real_, length(x))
  ok <- !is.na(x) & grepl(pattern, x)
  if (any(ok)) {
    parts <- regmatches(x[ok], regexec(pattern, x[ok]))
    secs[ok] <- vapply(parts, function(p) {
      as.numeric(ISOdatetime(
        as.integer(p[[4]]), match(p[[3]], month_abbr), as.integer(p[[2]]),
        as.integer(p[[5]]), as.integer(p[[6]]), as.integer(p[[7]]),
        tz = "UTC"
      ))
    }, numeric(1))
  }
  as.POSIXct(secs, origin = "1970-01-01", tz = "UTC")
}

format_http_date <- function(t) {
  t <- as.POSIXlt(t, tz = "UTC")
  sprintf(
    "%s, %02d %s %04d %02d:%02d:%02d GMT",
    day_abbr[t$wday + 1L], t$mday, month_abbr[t$mon + 1L], t$year + 1900L,
    t$hour, t$min, as.integer(t$sec)
  )
}

source_request <- function(url, cfg) {
  httr2::request(url) |>
    httr2::req_user_agent(cfg$source$user_agent) |>
    httr2::req_timeout(cfg$source$timeout_seconds) |>
    httr2::req_retry(max_tries = cfg$source$max_tries, backoff = function(i) min(60, 2^i))
}

probe_sources <- function(catalog, cfg) {
  remote <- is_remote_url(catalog$url)
  out <- tibble::tibble(
    id = catalog$id,
    url = catalog$url,
    etag = NA_character_,
    last_modified = NA_character_,
    bytes = NA_real_
  )
  if (any(!remote)) {
    info <- file.info(catalog$url[!remote])
    missing <- catalog$url[!remote][is.na(info$size)]
    if (length(missing) > 0) {
      cli::cli_abort("Source files not found: {.path {missing}}")
    }
    out$last_modified[!remote] <- format_http_date(info$mtime)
    out$bytes[!remote] <- info$size
  }
  if (any(remote)) {
    reqs <- lapply(catalog$url[remote], function(u) httr2::req_method(source_request(u, cfg), "HEAD"))
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = cfg$source$parallel_probes)
    failed <- vapply(resps, function(r) !inherits(r, "httr2_response"), logical(1))
    if (any(failed)) {
      cli::cli_abort("Could not reach {sum(failed)} source file{?s}: {.url {catalog$url[remote][failed]}}")
    }
    out$etag[remote] <- vapply(resps, function(r) httr2::resp_header(r, "etag", NA_character_), character(1))
    out$last_modified[remote] <- vapply(resps, function(r) httr2::resp_header(r, "last-modified", NA_character_), character(1))
    out$bytes[remote] <- vapply(resps, function(r) as.numeric(httr2::resp_header(r, "content-length", NA_character_)), numeric(1))
  }
  out
}

landing_is_current <- function(path, remote) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  info <- file.info(path)
  same_size <- is.na(remote$bytes) || info$size == remote$bytes
  stamp <- parse_http_date(remote$last_modified)
  fresh <- is.na(stamp) || as.numeric(info$mtime) >= as.numeric(stamp) - 1
  same_size && fresh
}

fetch_source <- function(src, remote, cfg) {
  dest <- data_dir(cfg, "landing", src$file)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (landing_is_current(dest, remote)) {
    return(dest)
  }
  part <- paste0(dest, ".part")
  unlink(part)
  if (is_remote_url(src$url)) {
    source_request(src$url, cfg) |> httr2::req_perform(path = part)
  } else if (!file.copy(src$url, part, overwrite = TRUE, copy.date = TRUE)) {
    cli::cli_abort("Could not copy {.path {src$url}}")
  }
  size <- file.info(part)$size
  if (!is.na(remote$bytes) && size != remote$bytes) {
    unlink(part)
    cli::cli_abort("{src$file}: got {size} bytes, expected {remote$bytes}")
  }
  stamp <- parse_http_date(remote$last_modified)
  if (!is.na(stamp)) {
    Sys.setFileTime(part, stamp)
  }
  if (!file.rename(part, dest)) {
    cli::cli_abort("Could not move {.path {part}} into the landing zone")
  }
  dest
}
