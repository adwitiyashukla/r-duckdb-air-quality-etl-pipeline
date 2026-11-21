config_file <- file.path(project_root(), "config.yml")

test_that("the shipped config and its live profile validate", {
  cfg <- read_config(config_file)
  expect_equal(cfg$years, c(2016L, 2025L))
  live <- read_config(config_file, profile = "live")
  expect_equal(live$years, c(2024L, 2024L))
  expect_equal(live$duckdb$threads, 2L)
  expect_equal(live$source$base_url, cfg$source$base_url)
})

test_that("bad settings are refused with the name of the setting", {
  cfg <- read_config(config_file)
  change <- function(path, value) {
    x <- cfg
    x[[path[1]]][[path[2]]] <- value
    x
  }
  bad <- cfg
  bad$years <- c(2025L, 2016L)
  expect_error(validate_config(bad), "first <= last")
  bad$years <- c(2016L, 2016L, 2017L)
  expect_error(validate_config(bad), "years")
  expect_error(validate_config(change(c("duckdb", "memory_limit"), "lots")), "duckdb.memory_limit")
  expect_error(validate_config(change(c("duckdb", "threads"), 0)), "duckdb.threads")
  expect_error(validate_config(change(c("source", "max_tries"), -1)), "source.max_tries")
  expect_error(validate_config(change(c("quality", "county_match_min"), 1.5)), "quality.county_match_min")
  expect_error(validate_config(change(c("bounds", "44201"), c(1, 0))), "low < high")
  bad <- cfg
  bad$bounds[["44201"]] <- NULL
  expect_error(validate_config(bad), "bounds")
})

test_that("a missing config file is a clear error", {
  expect_error(read_config(file.path(tempdir(), "nope.yml")), "not\\s+found")
})

test_that("trailing slashes are dropped from the source url", {
  cfg <- read_config(config_file)
  cfg$source$base_url <- "https://example.org/airdata///"
  expect_equal(validate_config(cfg)$source$base_url, "https://example.org/airdata")
})
