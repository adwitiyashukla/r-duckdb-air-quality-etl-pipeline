project_root <- function() {
  normalizePath(testthat::test_path("..", ".."), winslash = "/")
}

load_project <- function() {
  for (f in list.files(file.path(project_root(), "R"), pattern = "[.]R$", full.names = TRUE)) {
    sys.source(f, envir = globalenv())
  }
}

load_project()

fixture_sites <- tibble::tibble(
  site_id = c("39-061-0010", "39-061-0040", "39-017-0004", "39-017-0018"),
  county_name = c("Hamilton", "Hamilton", "Butler", "Butler")
)

hourly_frame <- function(site, code, poc, day, hours, values, mdl = 0.005) {
  ts <- as.POSIXct(sprintf("%s %02d:00:00", day, hours), tz = "UTC")
  parts <- strsplit(site, "-", fixed = TRUE)[[1]]
  p <- pollutants()
  data.frame(
    "State Code" = parts[[1]], "County Code" = parts[[2]], "Site Num" = parts[[3]], "Parameter Code" = code,
    "POC" = poc, "Latitude" = 39.1, "Longitude" = -84.5, "Datum" = "WGS84", "Parameter Name" = code,
    "Date Local" = format(ts, "%Y-%m-%d"), "Time Local" = format(ts, "%H:%M"),
    "Date GMT" = format(ts + 5 * 3600, "%Y-%m-%d"), "Time GMT" = format(ts + 5 * 3600, "%H:%M"),
    "Sample Measurement" = values, "Units of Measure" = p$units[p$parameter_code == code], "MDL" = mdl,
    "Uncertainty" = "", "Qualifier" = "", "Method Type" = "FEM", "Method Code" = "087", "Method Name" = "TEST",
    "State Name" = "Ohio", "County Name" = fixture_sites$county_name[match(site, fixture_sites$site_id)],
    "Date of Last Change" = "2024-06-01",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

fixture_hourly <- function() {
  o <- function(day, hours, values, site = "39-061-0010") hourly_frame(site, "44201", 1L, day, hours, values)
  list(
    "44201_2024" = rbind(
      o("2024-01-02", 0:23, 0.030 + 0.001 * (0:23 %% 5)),
      o("2024-01-03", 0:23, c(rep(0.060, 7), 0.030 + 0.001 * (7:23 %% 5))),
      o("2024-01-04", 7:18, rep(0.045, 12)),
      o("2024-01-02", 0:23, c(rep(0.050, 8), rep(0.040, 16)), site = "39-017-0004"),
      o("2024-01-03", 0:23, rep(0.040, 24), site = "39-017-0004")
    ),
    "88101_2024" = rbind(
      hourly_frame("39-061-0040", "88101", 3L, "2024-01-02", 0:23, 10 + (0:23) / 10, 2),
      hourly_frame("39-061-0040", "88101", 3L, "2024-01-03", 0:16, rep(20, 17), 2),
      hourly_frame("39-061-0040", "88101", 3L, "2024-01-04", 0:7, rep(50, 8), 2)
    ),
    "88502_2024" = rbind(
      hourly_frame("39-061-0010", "88502", 5L, "2024-01-02", 0:23, rep(8, 24), 2),
      hourly_frame("39-017-0018", "88502", 1L, "2024-01-02", 0:23, rep(8.3, 24), 2)
    ),
    "81102_2024" = rbind(
      hourly_frame("39-061-0040", "81102", 1L, "2024-01-02", 0:23, rep(40, 24), 5),
      hourly_frame("39-061-0040", "81102", 1L, "2024-01-03", 0:15, rep(300, 16), 5)
    ),
    "42101_2024" = rbind(
      hourly_frame("39-061-0010", "42101", 1L, "2024-01-01", 0:23, 0.4 + 0.05 * (0:23 %% 3), 0.02),
      hourly_frame("39-061-0010", "42101", 1L, "2024-01-02", 0:23, rep(0.3, 24), 0.02),
      hourly_frame("39-061-0010", "42101", 1L, "2024-01-03", 0:23, c(rep(0, 4), rep(-0.1, 4), rep(0, 16)), 0.02)
    ),
    "42401_2024" = hourly_frame("39-017-0004", "42401", 1L, "2024-01-02", 0:23, c(rep(5, 12), 400, rep(5, 11)), 0.5),
    "42602_2024" = rbind(
      hourly_frame("39-017-0004", "42602", 1L, "2024-01-02", 0:23, 20 + (0:23), 1),
      hourly_frame("39-017-0004", "42602", 1L, "2024-01-03", 0:16, rep(60, 17), 1)
    )
  )
}

ref_trunc <- function(x, d) sign(x) * floor(abs(x) * 10^d + 1e-7) / 10^d

ref_windows <- function(h, forward) {
  h$ts <- as.POSIXct(paste(h[["Date Local"]], h[["Time Local"]]), tz = "UTC")
  h$v <- as.numeric(h[["Sample Measurement"]])
  out <- list()
  for (key in unique(paste(h[["State Code"]], h[["County Code"]], h[["Site Num"]], h$POC))) {
    g <- h[paste(h[["State Code"]], h[["County Code"]], h[["Site Num"]], h$POC) == key, ]
    grid <- seq(min(g$ts) - 7 * 3600, max(g$ts) + 7 * 3600, by = 3600)
    for (k in seq_along(grid)) {
      span <- if (forward) grid[k] + (0:7) * 3600 else grid[k] - (7:0) * 3600
      vals <- g$v[match(span, g$ts)]
      n <- sum(!is.na(vals))
      if (n >= 6) {
        avg <- mean(vals, na.rm = TRUE)
        value <- if (forward) ref_trunc(avg, 3) else sign(avg) * floor(abs(avg) * 10 + 0.5 + 1e-7) / 10
        out[[length(out) + 1]] <- data.frame(
          site_id = paste(g[["State Code"]][1], g[["County Code"]][1], g[["Site Num"]][1], sep = "-"),
          poc = g$POC[1], stamp = grid[k], n = n, value = value
        )
      }
    }
  }
  do.call(rbind, out)
}

ref_area_inputs <- function(hourly, bp) {
  acc <- new.env()
  acc$rows <- list()
  add <- function(site, code, poc, day, conc, param) {
    p <- pollutants()
    table <- p$breakpoints[p$parameter_code == code]
    digits <- p$decimals[p$parameter_code == code]
    acc$rows[[length(acc$rows) + 1]] <- data.frame(
      site_id = site, parameter_code = code, poc = poc, date_local = as.Date(day), aqi_parameter = param,
      concentration = conc, aqi = aqi_value(conc, table, bp, digits)
    )
  }
  o3 <- ref_windows(hourly[["44201_2024"]], TRUE)
  o3$day <- as.Date(format(o3$stamp, "%Y-%m-%d"))
  o3$hour <- as.integer(format(o3$stamp, "%H"))
  for (key in unique(paste(o3$site_id, o3$poc, o3$day))) {
    g <- o3[paste(o3$site_id, o3$poc, o3$day) == key, ]
    if (format(g$day[1], "%Y") != "2024") next
    n24 <- nrow(g)
    n17 <- sum(g$hour >= 7)
    conc <- if (n24 >= 18) max(g$value) else if (n17 >= 13) max(g$value[g$hour >= 7]) else NA
    if (!is.na(conc)) add(g$site_id[1], "44201", g$poc[1], g$day[1], conc, "Ozone")
  }
  co <- ref_windows(hourly[["42101_2024"]], FALSE)
  co$day <- as.Date(format(co$stamp, "%Y-%m-%d"))
  co <- co[format(co$day, "%Y") == "2024", ]
  for (key in unique(paste(co$site_id, co$poc, co$day))) {
    g <- co[paste(co$site_id, co$poc, co$day) == key, ]
    add(g$site_id[1], "42101", g$poc[1], g$day[1], max(g$value), "CO")
  }
  daily_stats <- function(h) {
    h$v <- as.numeric(h[["Sample Measurement"]])
    h$site_id <- paste(h[["State Code"]], h[["County Code"]], h[["Site Num"]], sep = "-")
    stats::aggregate(
      v ~ site_id + POC + `Date Local`, data = h,
      FUN = function(x) c(n = length(x), mean = mean(x), sum = sum(x), max = max(x))
    )
  }
  for (code in c("88101", "88502", "81102")) {
    s <- daily_stats(hourly[[paste0(code, "_2024")]])
    for (i in seq_len(nrow(s))) {
      st <- s$v[i, ]
      conc <- if (code == "81102") {
        block <- ref_trunc(st[["sum"]] / 24, 0)
        if (st[["n"]] >= 18) ref_trunc(st[["mean"]], 0) else if (block > 150) block else NA
      } else {
        if (st[["n"]] >= 18) ref_trunc(st[["mean"]], 1) else if (ref_trunc(st[["mean"]], 1) > 35) ref_trunc(st[["mean"]], 1) else NA
      }
      if (!is.na(conc)) add(s$site_id[i], code, s$POC[i], s[["Date Local"]][i], conc, if (code == "81102") "PM10" else "PM2.5")
    }
  }
  add("39-061-0040", "88101", 1L, "2024-01-05", 15.2, "PM2.5")
  s <- daily_stats(hourly[["42602_2024"]])
  for (i in seq_len(nrow(s))) {
    st <- s$v[i, ]
    if (st[["n"]] >= 18 || st[["max"]] > 100) add(s$site_id[i], "42602", s$POC[i], s[["Date Local"]][i], st[["max"]], "NO2")
  }
  do.call(rbind, acc$rows)
}

ref_area <- function(inputs, keys) {
  groups <- split(inputs, inputs[, c(keys, "date_local")], drop = TRUE)
  out <- lapply(groups, function(g) {
    top <- g[g$aqi == max(g$aqi), ]
    cbind(
      g[1, c(keys, "date_local")], aqi = top$aqi[1], category = aqi_category(top$aqi[1]),
      defining_parameter = min(top$aqi_parameter), defining_site = max(top$site_id), n_sites = length(unique(g$site_id))
    )
  })
  do.call(rbind, out)
}

write_epa_zip <- function(dir, stem, df) {
  csv <- file.path(tempdir(), paste0(stem, ".csv"))
  utils::write.csv(df, csv, row.names = FALSE, na = "")
  zipfile <- file.path(dir, paste0(stem, ".zip"))
  unlink(zipfile)
  zip::zipr(zipfile, csv)
  unlink(csv)
  zipfile
}

empty_frame <- function(kind) {
  cols <- source_header(kind)
  stats::setNames(as.data.frame(matrix(character(0), ncol = length(cols))), cols)
}

daily_frame <- function(site, code, poc, duration, standard, day, obs, mean, max_value, max_hour, aqi) {
  parts <- strsplit(site, "-", fixed = TRUE)[[1]]
  p <- pollutants()
  data.frame(
    "State Code" = parts[[1]], "County Code" = parts[[2]], "Site Num" = parts[[3]], "Parameter Code" = code,
    "POC" = poc, "Latitude" = 39.1, "Longitude" = -84.5, "Datum" = "WGS84", "Parameter Name" = code,
    "Sample Duration" = duration, "Pollutant Standard" = standard, "Date Local" = format(as.Date(day)),
    "Units of Measure" = p$units[p$parameter_code == code], "Event Type" = "None", "Observation Count" = obs,
    "Observation Percent" = 100, "Arithmetic Mean" = mean, "1st Max Value" = max_value, "1st Max Hour" = max_hour,
    "AQI" = aqi, "Method Code" = "087", "Method Name" = "TEST", "Local Site Name" = "", "Address" = "",
    "State Name" = "Ohio", "County Name" = fixture_sites$county_name[match(site, fixture_sites$site_id)],
    "City Name" = "Cincinnati", "CBSA Name" = "Cincinnati, OH-KY-IN", "Date of Last Change" = "2024-06-01",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

build_fixture_source <- function(dir, plant_mismatch = TRUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  bp <- read_breakpoints(file.path(project_root(), "ref", "aqi_breakpoints.csv"))
  hourly <- fixture_hourly()
  for (code in pollutant_codes()) {
    key <- paste0(code, "_2024")
    df <- if (is.null(hourly[[key]])) empty_frame("hourly") else hourly[[key]]
    write_epa_zip(dir, paste0("hourly_", key), df)
  }

  inputs <- ref_area_inputs(hourly, bp)
  o3 <- ref_windows(hourly[["44201_2024"]], TRUE)
  o3 <- o3[format(o3$stamp, "%Y") == "2024" & as.integer(format(o3$stamp, "%H")) >= 7, ]
  eight <- data.frame(
    "State Code" = substr(o3$site_id, 1, 2), "County Code" = substr(o3$site_id, 4, 6), "Site Num" = substr(o3$site_id, 8, 11),
    "Parameter Code" = "44201", "POC" = o3$poc, "Latitude" = 39.1, "Longitude" = -84.5, "Datum" = "WGS84",
    "Parameter Name" = "Ozone", "Date Local" = format(o3$stamp, "%Y-%m-%d"), "Time Local" = format(o3$stamp, "%H:%M"),
    "Date GMT" = format(o3$stamp + 5 * 3600, "%Y-%m-%d"), "Time GMT" = format(o3$stamp + 5 * 3600, "%H:%M"),
    "Sample Duration" = "8-HR RUN AVG BEGIN HOUR", "Pollutant Standard" = "Ozone 8-hour 2015",
    "Units of Measure" = "Parts per million", "Observation Count" = o3$n, "Observations with Events" = 0,
    "Null Observations" = 0, "Mean Including All Data" = o3$value, "Mean Excluding All Flagged Data" = o3$value,
    "Mean Excluding Concurred Flags" = o3$value, "Date of Last Change" = "2024-06-01",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  write_epa_zip(dir, "8hour_44201_2024", eight)

  o3$day <- format(o3$stamp, "%Y-%m-%d")
  daily <- list()
  for (key in unique(paste(o3$site_id, o3$poc, o3$day))) {
    g <- o3[paste(o3$site_id, o3$poc, o3$day) == key, ]
    top <- g[order(-g$value, g$stamp), ][1, ]
    daily[[length(daily) + 1]] <- daily_frame(
      g$site_id[1], "44201", g$poc[1], "8-HR RUN AVG BEGIN HOUR", "Ozone 8-hour 2015", g$day[1], nrow(g),
      round(mean(g$value), 6), top$value, as.integer(format(top$stamp, "%H")), aqi_value(top$value, "ozone_8h", bp, 3L)
    )
  }
  pm <- inputs[inputs$parameter_code %in% c("88101", "88502", "81102"), ]
  for (i in seq_len(nrow(pm))) {
    duration <- if (pm$poc[i] == 1L && pm$parameter_code[i] == "88101") "24 HOUR" else "24-HR BLK AVG"
    daily[[length(daily) + 1]] <- daily_frame(
      pm$site_id[i], pm$parameter_code[i], pm$poc[i], duration, "", pm$date_local[i], 1,
      pm$concentration[i], pm$concentration[i], 0, pm$aqi[i]
    )
  }
  daily <- do.call(rbind, daily)
  for (code in pollutant_codes()) {
    df <- daily[daily[["Parameter Code"]] == code, ]
    if (nrow(df) == 0) df <- empty_frame("daily")
    write_epa_zip(dir, paste0("daily_", code, "_2024"), df)
  }

  inputs$state_code <- substr(inputs$site_id, 1, 2)
  inputs$county_code <- substr(inputs$site_id, 4, 6)
  county <- ref_area(inputs, c("state_code", "county_code"))
  if (plant_mismatch) {
    hit <- county$county_code == "017" & county$date_local == as.Date("2024-01-03")
    county$aqi[hit] <- county$aqi[hit] + 3L
  }
  write_epa_zip(dir, "daily_aqi_by_county_2024", data.frame(
    "State Name" = "Ohio", "county Name" = ifelse(county$county_code == "061", "Hamilton", "Butler"),
    "State Code" = county$state_code, "County Code" = county$county_code, "Date" = format(county$date_local),
    "AQI" = county$aqi, "Category" = aqi_category(county$aqi), "Defining Parameter" = county$defining_parameter,
    "Defining Site" = county$defining_site, "Number of Sites Reporting" = county$n_sites,
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  inputs$cbsa_code <- "17140"
  cbsa <- ref_area(inputs, "cbsa_code")
  write_epa_zip(dir, "daily_aqi_by_cbsa_2024", data.frame(
    "CBSA" = "Cincinnati, OH-KY-IN", "CBSA Code" = cbsa$cbsa_code, "Date" = format(cbsa$date_local),
    "AQI" = cbsa$aqi, "Category" = cbsa$category, "Defining Parameter" = cbsa$defining_parameter,
    "Defining Site" = cbsa$defining_site, "Number of Sites Reporting" = cbsa$n_sites,
    check.names = FALSE, stringsAsFactors = FALSE
  ))

  sites <- empty_frame("sites")[rep(1, nrow(fixture_sites)), ]
  sites[] <- ""
  sites[["State Code"]] <- substr(fixture_sites$site_id, 1, 2)
  sites[["County Code"]] <- substr(fixture_sites$site_id, 4, 6)
  sites[["Site Number"]] <- substr(fixture_sites$site_id, 8, 11)
  sites[["Latitude"]] <- "39.1"
  sites[["Longitude"]] <- "-84.5"
  sites[["GMT Offset"]] <- "-5"
  sites[["State Name"]] <- "Ohio"
  sites[["County Name"]] <- fixture_sites$county_name
  sites[["CBSA Name"]] <- "Cincinnati, OH-KY-IN"
  write_epa_zip(dir, "aqs_sites", sites)
  monitors <- empty_frame("monitors")[rep(1, 2), ]
  monitors[] <- ""
  monitors[["State Code"]] <- "39"
  monitors[["County Code"]] <- "061"
  monitors[["Site Number"]] <- c("0010", "0040")
  monitors[["Parameter Code"]] <- c("44201", "88101")
  monitors[["POC"]] <- c("1", "3")
  write_epa_zip(dir, "aqs_monitors", monitors)
  list(dir = dir, inputs = inputs, county = county, cbsa = cbsa)
}

fixture_config <- function(data_dir, source_dir, years = c(2024L, 2024L)) {
  cfg <- read_config(file.path(project_root(), "config.yml"))
  cfg$years <- as.integer(years)
  cfg$source$base_url <- source_dir
  cfg$paths$data <- data_dir
  cfg$paths$breakpoints <- file.path(project_root(), "ref", "aqi_breakpoints.csv")
  cfg$duckdb$memory_limit <- "1GB"
  cfg$duckdb$threads <- 1L
  cfg$quality$daily_match_min <- 0.9
  cfg$quality$county_match_min <- 0.6
  cfg$quality$cbsa_match_min <- 0.6
  cfg
}

fixture_cache <- new.env()

fixture_warehouse <- function() {
  if (is.null(fixture_cache$path)) {
    root <- file.path(tempdir(), "fixture-project")
    unlink(root, recursive = TRUE)
    dir.create(root)
    make_fixture_project(root)
    withr::with_dir(root, targets::tar_make(reporter = "silent"))
    fixture_cache$path <- file.path(root, "data", "warehouse.duckdb")
  }
  fixture_cache$path
}

make_fixture_project <- function(root) {
  for (d in c("R", "ref")) {
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(file.path(project_root(), d), full.names = TRUE), file.path(root, d))
  }
  file.copy(file.path(project_root(), "_targets.R"), root)
  fx <- build_fixture_source(file.path(root, "source"))
  cfg <- yaml::read_yaml(file.path(project_root(), "config.yml"))
  cfg$default$years <- c(2024L, 2024L)
  cfg$default$source$base_url <- fx$dir
  cfg$default$duckdb$memory_limit <- "1GB"
  cfg$default$duckdb$threads <- 1L
  cfg$default$quality$county_match_min <- 0.6
  cfg$default$quality$cbsa_match_min <- 0.6
  cfg$default$quality$daily_match_min <- 0.9
  yaml::write_yaml(cfg, file.path(root, "config.yml"))
  fx
}
