fixture_bronze <- function(root) {
  fx <- build_fixture_source(file.path(root, "source"))
  cfg <- fixture_config(file.path(root, "data"), fx$dir)
  cat <- source_catalog(cfg)
  remote <- probe_sources(cat, cfg)
  paths <- stats::setNames(vapply(seq_len(nrow(cat)), function(i) {
    ingest_source(cat[i, ], remote[i, ], cfg)
  }, character(1)), cat$id)
  list(cfg = cfg, fx = fx, bronze = paths, bp = read_breakpoints(cfg$paths$breakpoints))
}

rebuild_all <- function(b) {
  unlist(lapply(pollutant_codes(), function(code) {
    hourly <- b$bronze[[paste0("hourly_", code, "_2024")]]
    rebuild_pollutant(b$cfg, code, 2024L, hourly, b$bronze[[paste0("daily_", code, "_2024")]], b$bp)
  }), use.names = FALSE)
}

read_inputs <- function(b, files) {
  with_duck(b$cfg, function(con) {
    duck_get(con, paste0(area_inputs_sql(pick_file(files, "aqi")), " order by site_id, parameter_code, poc, date_local"))
  })
}

test_that("the DuckDB rebuild agrees with the plain R reference on every monitor-day", {
  b <- fixture_bronze(withr::local_tempdir())
  got <- read_inputs(b, rebuild_all(b))
  want <- b$fx$inputs
  key <- function(d) paste(d$site_id, d$parameter_code, d$poc, format(as.Date(d$date_local)))
  expect_setequal(key(got), key(want))
  m <- match(key(want), key(got))
  expect_equal(got$aqi[m], want$aqi)
  expect_equal(got$concentration[m], want$concentration, tolerance = 1e-9)
})

test_that("hand checked days come out as expected", {
  b <- fixture_bronze(withr::local_tempdir())
  got <- read_inputs(b, rebuild_all(b))
  day <- function(site, code, d) got[got$site_id == site & got$parameter_code == code & format(as.Date(got$date_local)) == d, ]
  early <- day("39-061-0010", "44201", "2024-01-03")
  expect_equal(early$concentration, 0.056)
  expect_equal(early$aqi, 54L)
  expect_equal(nrow(day("39-061-0010", "44201", "2024-01-04")), 0L)
  expect_equal(nrow(day("39-061-0040", "88101", "2024-01-03")), 0L)
  expect_equal(day("39-061-0040", "88101", "2024-01-04")$aqi, 137L)
  expect_equal(day("39-061-0040", "81102", "2024-01-03")$concentration, 200)
  expect_equal(day("39-061-0040", "81102", "2024-01-03")$aqi, 123L)
  expect_equal(nrow(day("39-017-0004", "42602", "2024-01-03")), 0L)
  expect_equal(nrow(got[got$parameter_code == "42401", ]), 0L)
})

test_that("the rebuilt 8 hour ozone averages match the reference file exactly", {
  b <- fixture_bronze(withr::local_tempdir())
  files <- rebuild_all(b)
  out <- reconcile_eighthour(b$cfg, pick_file(files, "eighthour"), b$bronze[["eighthour_2024"]], 2024L)
  s <- with_duck(b$cfg, function(con) {
    duck_get(con, paste0("select status, sum(n) as n from read_parquet(", sql_path(out[["summary"]]), ") group by all"))
  })
  expect_equal(s$status, "match")
})

test_that("county and CBSA rollups break ties like EPA and find the planted difference", {
  b <- fixture_bronze(withr::local_tempdir())
  files <- rebuild_all(b)
  daily <- unname(b$bronze[paste0("daily_", pollutant_codes(), "_2024")])
  area <- build_area_aqi(b$cfg, pick_file(files, "aqi"), daily, b$bronze[["aqi_cbsa_2024"]], 2024L)
  county <- reconcile_area(b$cfg, "county", area[["county"]], b$bronze[["aqi_county_2024"]], pick_file(files, "aqi"), 2024L)
  cbsa <- reconcile_area(b$cfg, "cbsa", area[["cbsa"]], b$bronze[["aqi_cbsa_2024"]], pick_file(files, "aqi"), 2024L)
  with_duck(b$cfg, function(con) {
    cs <- duck_get(con, paste0("select status, reason, n from read_parquet(", sql_path(county[["summary"]]), ")"))
    expect_equal(sum(cs$n[cs$status != "match"]), 1)
    expect_equal(cs$reason[cs$status == "aqi_differs"], "epa_defining_site_lower_in_rebuild")
    bs <- duck_get(con, paste0("select status, n from read_parquet(", sql_path(cbsa[["summary"]]), ")"))
    expect_equal(unique(bs$status), "match")
    tie <- duck_get(con, paste0(
      "select defining_parameter, defining_site, n_sites from read_parquet(", sql_path(area[["county"]]), ") ",
      "where county_code = '017' and date_local = date '2024-01-02'"
    ))
    expect_equal(tie$defining_parameter, "Ozone")
    expect_equal(tie$defining_site, "39-017-0018")
    expect_equal(tie$n_sites, 2L)
  })
})
