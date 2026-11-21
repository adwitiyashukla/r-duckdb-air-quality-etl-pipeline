bp <- read_breakpoints(file.path(project_root(), "ref", "aqi_breakpoints.csv"))

test_that("aqi matches the worked examples in the EPA technical assistance document", {
  expect_equal(aqi_value(0.07853333, "ozone_8h", bp, 3L), 126L)
  expect_equal(aqi_value(35.9, "pm25_24h", bp, 1L), 102L)
  expect_equal(aqi_value(8.4, "co_8h", bp, 1L), 90L)
})

test_that("concentrations are truncated before the breakpoint lookup", {
  expect_equal(aqi_value(0.0549999, "ozone_8h", bp, 3L), 50L)
  expect_equal(aqi_value(0.055, "ozone_8h", bp, 3L), 51L)
  expect_equal(aqi_value(9.09, "pm25_24h", bp, 1L), 50L)
  expect_equal(aqi_value(9.1, "pm25_24h", bp, 1L), 51L)
  expect_equal(aqi_value(54.9, "pm10_24h", bp, 0L), 50L)
})

test_that("negatives clamp to zero, the top segment extrapolates and 1 hour SO2 stops at 200", {
  expect_equal(aqi_value(-3.2, "pm25_24h", bp, 1L), 0L)
  expect_equal(aqi_value(833.8, "pm25_24h", bp, 1L), 1513L)
  expect_equal(aqi_value(372.1, "pm25_24h", bp, 1L), 593L)
  expect_equal(aqi_value(1343, "pm10_24h", bp, 0L), 1322L)
  expect_equal(aqi_value(438, "so2_1h", bp, 0L), 200L)
  expect_equal(aqi_value(304, "so2_1h", bp, 0L), 200L)
  expect_true(is.na(aqi_value(NA_real_, "no2_1h", bp, 0L)))
})

test_that("the 2024 PM2.5 breakpoints read the same air as dirtier than the 2012 ones", {
  expect_equal(aqi_value(10.0, "pm25_24h_2012", bp, 1L), 42L)
  expect_equal(aqi_value(10.0, "pm25_24h", bp, 1L), 53L)
})

test_that("the SQL and R versions of the AQI agree on random concentrations", {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  define_macros(con)
  set.seed(7)
  p <- unique(pollutants()[, c("breakpoints", "decimals")])
  for (i in seq_len(nrow(p))) {
    hi <- max(bp$conc_hi[bp$table == p$breakpoints[i]]) * 1.3
    x <- round(stats::runif(400, -0.05 * hi, hi), p$decimals[i] + 2)
    duckdb::duckdb_register(con, "x", data.frame(c = x), overwrite = TRUE)
    got <- DBI::dbGetQuery(con, paste0("select ", aqi_sql("c", p$breakpoints[i], bp, p$decimals[i]), " as a from x"))$a
    expect_equal(got, aqi_value(x, p$breakpoints[i], bp, p$decimals[i]), info = p$breakpoints[i])
  }
})

test_that("rounding goes half away from zero, the way EPA rounds CO averages", {
  expect_equal(round_half_up(c(0.05, -0.05, 0.15, -0.15, -0.0375, 0.0625), 1), c(0.1, -0.1, 0.2, -0.2, 0, 0.1))
  expect_equal(round_half_up(c(-0.4, 0.3 - 0.25) / c(8, 1), 1), c(-0.1, 0.1))
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  define_macros(con)
  x <- round(seq(-1, 1, by = 0.0125), 4)
  duckdb::duckdb_register(con, "x", data.frame(v = x))
  expect_equal(DBI::dbGetQuery(con, "select round_half_up(v, 1) as r from x")$r, round_half_up(x, 1))
})

test_that("categories follow the AQI bands", {
  expect_equal(
    aqi_category(c(0, 50, 51, 100, 101, 150, 151, 200, 201, 300, 301, 900)),
    rep(c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"), each = 2)
  )
})

test_that("a broken breakpoint table is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  bad <- bp
  bad$conc_lo[2] <- 100
  utils::write.csv(bad, path, row.names = FALSE)
  expect_error(read_breakpoints(path), "strictly increasing")
})
