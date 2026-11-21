open_warehouse <- function(path = "data/warehouse.duckdb") {
  if (!file.exists(path)) {
    cli::cli_abort("Warehouse {.path {path}} not found, run the pipeline first")
  }
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = path, read_only = TRUE)
}

bad_request <- function(res, message) {
  res$status <- 400L
  list(error = message)
}

valid_code <- function(x, digits) {
  is.character(x) && length(x) == 1 && grepl(sprintf("^[0-9A-Z]{%d}$", digits), x)
}

valid_date <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) && !is.na(as.Date(x, optional = TRUE))
}

query_rows <- function(con, sql, params = list()) {
  rows <- DBI::dbGetQuery(con, sql, params = params)
  for (col in names(rows)) {
    if (inherits(rows[[col]], "Date")) {
      rows[[col]] <- format(rows[[col]])
    }
  }
  rows
}

api_health <- function(con) {
  info <- query_rows(con, "select key, value from build_info")
  counts <- query_rows(con, paste0(
    "select count(*) as county_days, count(*) filter (where status in ('match', 'aqi_match')) as aqi_matched ",
    "from county_aqi"
  ))
  c(list(status = "ok"), stats::setNames(as.list(info$value), info$key), as.list(counts))
}

api_counties <- function(con, state) {
  query_rows(con, paste0(
    "select state_code, county_code, any_value(state_name) as state_name, any_value(county_name) as county_name, ",
    "count(aqi) as days from county_aqi where state_code = ? group by all order by county_code"
  ), list(state))
}

api_county_series <- function(con, state, county, from, to) {
  query_rows(con, paste0(
    "select date_local, aqi, category, defining_parameter, defining_site, n_sites, epa_aqi, epa_category, ",
    "epa_defining_parameter, epa_defining_site, status, reason from county_aqi ",
    "where state_code = ? and county_code = ? and date_local between ?::date and ?::date order by date_local"
  ), list(state, county, from, to))
}

api_cbsa_series <- function(con, code, from, to) {
  query_rows(con, paste0(
    "select date_local, cbsa_name, aqi, category, defining_parameter, defining_site, n_sites, epa_aqi, status, reason ",
    "from cbsa_aqi where cbsa_code = ? and date_local between ?::date and ?::date order by date_local"
  ), list(code, from, to))
}

api_worst <- function(con, date, limit) {
  query_rows(con, paste0(
    "select state_code, county_code, state_name, county_name, aqi, category, defining_parameter, defining_site, epa_aqi ",
    "from county_aqi where date_local = ?::date and aqi is not null order by aqi desc, state_code, county_code limit ?"
  ), list(date, limit))
}

api_reconciliation <- function(con, level) {
  query_rows(con, paste0(
    "select year, status, sum(n)::bigint as n from recon_summary where level = ? group by all order by year, status"
  ), list(level))
}

api_mismatches <- function(con, level, year, limit) {
  rows <- query_rows(con, paste0(
    "select * exclude (level) from recon_mismatch where level = ? ",
    "and year(coalesce(date_local, cast(begin_local as date))) = ? ",
    "order by coalesce(date_local, cast(begin_local as date)) limit ?"
  ), list(level, as.integer(year), limit))
  if (nrow(rows) == 0) {
    return(rows)
  }
  rows[, vapply(rows, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
}

api_router <- function(con) {
  levels <- c("eighthour", "daily", "county", "cbsa")
  json <- plumber::serializer_unboxed_json()
  plumber::pr() |>
    plumber::pr_get("/health", function() api_health(con), serializer = json) |>
    plumber::pr_get("/counties", function(res, state = "") {
      if (!valid_code(state, 2)) {
        return(bad_request(res, "state must be a two digit state code like 06"))
      }
      api_counties(con, state)
    }) |>
    plumber::pr_get("/aqi/county", function(res, state = "", county = "", from = "", to = "") {
      if (!valid_code(state, 2) || !valid_code(county, 3)) {
        return(bad_request(res, "state and county must be codes like 06 and 037"))
      }
      if (!valid_date(from) || !valid_date(to) || from > to) {
        return(bad_request(res, "from and to must be dates like 2024-01-01 with from <= to"))
      }
      api_county_series(con, state, county, from, to)
    }) |>
    plumber::pr_get("/aqi/cbsa", function(res, code = "", from = "", to = "") {
      if (!valid_code(code, 5)) {
        return(bad_request(res, "code must be a five digit CBSA code like 31080"))
      }
      if (!valid_date(from) || !valid_date(to) || from > to) {
        return(bad_request(res, "from and to must be dates like 2024-01-01 with from <= to"))
      }
      api_cbsa_series(con, code, from, to)
    }) |>
    plumber::pr_get("/aqi/worst", function(res, date = "", limit = "10") {
      n <- suppressWarnings(as.integer(limit))
      if (!valid_date(date) || is.na(n) || n < 1 || n > 500) {
        return(bad_request(res, "date must look like 2024-01-01 and limit must be 1 to 500"))
      }
      api_worst(con, date, n)
    }) |>
    plumber::pr_get("/reconciliation", function(res, level = "county") {
      if (!level %in% levels) {
        return(bad_request(res, paste("level must be one of", paste(levels, collapse = ", "))))
      }
      api_reconciliation(con, level)
    }) |>
    plumber::pr_get("/mismatches", function(res, level = "county", year = "", limit = "100") {
      y <- suppressWarnings(as.integer(year))
      n <- suppressWarnings(as.integer(limit))
      if (!level %in% levels || is.na(y) || is.na(n) || n < 1 || n > 1000) {
        return(bad_request(res, "need level, a year and a limit between 1 and 1000"))
      }
      api_mismatches(con, level, y, n)
    })
}

run_api <- function(warehouse = "data/warehouse.duckdb", host = "127.0.0.1", port = 8000) {
  con <- open_warehouse(warehouse)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  plumber::pr_run(api_router(con), host = host, port = port, docs = TRUE)
}
