test_that("http dates round trip", {
  x <- "Mon, 13 Jul 2026 15:47:55 GMT"
  t <- parse_http_date(x)
  expect_equal(format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-07-13 15:47:55")
  expect_equal(format_http_date(t), x)
  expect_true(is.na(parse_http_date("yesterday")))
})

test_that("the catalog lists every file once and only for the configured years", {
  cfg <- fixture_config(tempdir(), tempdir(), years = c(2020L, 2021L))
  cat <- source_catalog(cfg)
  expect_false(anyDuplicated(cat$id) > 0)
  expect_equal(nrow(cat), 2 * (7 + 7 + 3) + 2)
  expect_true(all(c("hourly_42101_2020", "hourly_44201_2021", "sites", "monitors") %in% cat$id))
  expect_false(any(c("hourly_42101_2019", "hourly_44201_2022") %in% cat$id))
  expect_equal(cat$file[cat$id == "eighthour_2021"], "8hour_44201_2021.zip")
  expect_equal(cat$file[cat$id == "aqi_cbsa_2020"], "daily_aqi_by_cbsa_2020.zip")
})

test_that("local sources are probed, copied once and then served from the landing zone", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source")
  dir.create(src)
  writeBin(as.raw(1:200), file.path(src, "aqs_sites.zip"))
  cfg <- fixture_config(file.path(root, "data"), src)
  cat <- source_catalog(cfg)
  row <- cat[cat$id == "sites", ]
  remote <- probe_sources(row, cfg)
  expect_equal(remote$bytes, 200)
  first <- fetch_source(row, remote, cfg)
  expect_true(file.exists(first))
  unlink(file.path(src, "aqs_sites.zip"))
  expect_equal(fetch_source(row, remote, cfg), first)
  expect_error(probe_sources(row, cfg), "not found")
})

test_that("a download with the wrong size is thrown away", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source")
  dir.create(src)
  writeBin(as.raw(1:50), file.path(src, "aqs_monitors.zip"))
  cfg <- fixture_config(file.path(root, "data"), src)
  row <- source_catalog(cfg)
  row <- row[row$id == "monitors", ]
  remote <- probe_sources(row, cfg)
  remote$bytes <- 51
  expect_error(fetch_source(row, remote, cfg), "expected 51")
  expect_false(file.exists(file.path(root, "data", "landing", "aqs_monitors.zip")))
})

test_that("a landing copy is reused only while it is at least as new as the source", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source")
  dir.create(src)
  writeBin(as.raw(1:80), file.path(src, "aqs_sites.zip"))
  cfg <- fixture_config(file.path(root, "data"), src)
  row <- source_catalog(cfg)
  row <- row[row$id == "sites", ]
  dest <- file.path(root, "data", "landing", "aqs_sites.zip")
  dir.create(dirname(dest), recursive = TRUE)
  writeBin(as.raw(81:160), dest)
  remote <- probe_sources(row, cfg)
  remote$last_modified <- format_http_date(Sys.time() - 3600)
  fetch_source(row, remote, cfg)
  expect_equal(readBin(dest, "raw", 80), as.raw(81:160))
  remote$last_modified <- format_http_date(Sys.time() + 3600)
  fetch_source(row, remote, cfg)
  expect_equal(readBin(dest, "raw", 80), as.raw(1:80))
})
