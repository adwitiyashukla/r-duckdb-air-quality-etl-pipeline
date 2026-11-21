pollutants <- function() {
  tibble::tibble(
    parameter_code = c("44201", "88101", "88502", "81102", "42101", "42401", "42602"),
    aqi_parameter = c("Ozone", "PM2.5", "PM2.5", "PM10", "CO", "SO2", "NO2"),
    breakpoints = c("ozone_8h", "pm25_24h", "pm25_24h", "pm10_24h", "co_8h", "so2_1h", "no2_1h"),
    decimals = c(3L, 1L, 1L, 0L, 1L, 0L, 0L),
    in_area_aqi = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    units = c(
      "Parts per million", "Micrograms/cubic meter (LC)", "Micrograms/cubic meter (LC)",
      "Micrograms/cubic meter (25 C)", "Parts per million", "Parts per billion", "Parts per billion"
    )
  )
}

pollutant_codes <- function() {
  pollutants()$parameter_code
}

corrected_t640_methods <- function() {
  c("736", "738")
}

source_file_name <- function(kind, parameter_code, year) {
  switch(kind,
    hourly = sprintf("hourly_%s_%d.zip", parameter_code, year),
    daily = sprintf("daily_%s_%d.zip", parameter_code, year),
    eighthour = sprintf("8hour_44201_%d.zip", year),
    aqi_county = sprintf("daily_aqi_by_county_%d.zip", year),
    aqi_cbsa = sprintf("daily_aqi_by_cbsa_%d.zip", year),
    sites = "aqs_sites.zip",
    monitors = "aqs_monitors.zip",
    cli::cli_abort("Unknown source kind {.val {kind}}")
  )
}

source_id <- function(kind, parameter_code, year) {
  ifelse(
    kind %in% c("hourly", "daily"),
    sprintf("%s_%s_%d", kind, parameter_code, year),
    ifelse(kind %in% c("sites", "monitors"), kind, sprintf("%s_%d", kind, year))
  )
}

is_remote_url <- function(x) {
  grepl("^https?://", x)
}

source_catalog <- function(cfg) {
  years <- seq(cfg$years[[1]], cfg$years[[2]])
  codes <- pollutant_codes()
  grid <- function(kind, codes, years, role) {
    g <- expand.grid(parameter_code = codes, year = years, stringsAsFactors = FALSE)
    tibble::tibble(kind = kind, parameter_code = g$parameter_code, year = as.integer(g$year), role = role)
  }
  rows <- rbind(
    grid("hourly", codes, years, "core"),
    grid("daily", codes, years, "core"),
    grid("eighthour", "44201", years, "reference"),
    grid("aqi_county", NA_character_, years, "reference"),
    grid("aqi_cbsa", NA_character_, years, "reference"),
    tibble::tibble(kind = c("sites", "monitors"), parameter_code = NA_character_, year = NA_integer_, role = "listing")
  )
  rows$file <- mapply(source_file_name, rows$kind, rows$parameter_code, rows$year, USE.NAMES = FALSE)
  rows$id <- source_id(rows$kind, rows$parameter_code, rows$year)
  rows$url <- if (is_remote_url(cfg$source$base_url)) {
    paste(cfg$source$base_url, rows$file, sep = "/")
  } else {
    file.path(cfg$source$base_url, rows$file)
  }
  rows[, c("id", "kind", "parameter_code", "year", "role", "file", "url")]
}
