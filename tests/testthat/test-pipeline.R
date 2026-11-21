progress_of <- function(root) {
  p <- withr::with_dir(root, targets::tar_progress())
  stats::setNames(p$progress, p$name)
}

test_that("the pipeline runs end to end, then skips everything on a second run", {
  root <- withr::local_tempdir()
  make_fixture_project(root)
  withr::with_dir(root, targets::tar_make(reporter = "silent"))
  expect_true(file.exists(file.path(root, "data", "warehouse.duckdb")))

  con <- open_warehouse(file.path(root, "data", "warehouse.duckdb"))
  status <- DBI::dbGetQuery(con, "select status, reason, count(*) as n from county_aqi group by all")
  quality <- DBI::dbGetQuery(con, "select count(*) as n, count(*) filter (where stop) as stops from quality")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(status$reason[status$status == "aqi_differs"], "epa_defining_site_lower_in_rebuild")
  expect_gt(quality$n, 20)
  expect_equal(quality$stops, 0)

  withr::with_dir(root, targets::tar_make(reporter = "silent"))
  p <- progress_of(root)
  expect_equal(unname(p[["remote_index"]]), "completed")
  expect_true(all(p[names(p) != "remote_index"] == "skipped"))
})

test_that("a republished file only reruns its own branch", {
  root <- withr::local_tempdir()
  make_fixture_project(root)
  withr::with_dir(root, targets::tar_make(reporter = "silent"))
  df <- fixture_hourly()[["42602_2024"]]
  df[["Sample Measurement"]][df[["Date Local"]] == "2024-01-02"][5] <- 90
  write_epa_zip(file.path(root, "source"), "hourly_42602_2024", df)
  Sys.setFileTime(file.path(root, "source", "hourly_42602_2024.zip"), Sys.time() + 60)
  withr::with_dir(root, targets::tar_make(reporter = "silent"))
  p <- progress_of(root)
  expect_equal(unname(p[["bronze_hourly_42602_2024"]]), "completed")
  expect_equal(unname(p[["rebuild_42602_2024"]]), "completed")
  expect_equal(unname(p[["bronze_hourly_44201_2024"]]), "skipped")
  expect_equal(unname(p[["rebuild_44201_2024"]]), "skipped")
  expect_equal(unname(p[["recon_eighthour_2024"]]), "skipped")
})

test_that("a failed data contract stops the run before the warehouse", {
  root <- withr::local_tempdir()
  make_fixture_project(root)
  cfg <- yaml::read_yaml(file.path(root, "config.yml"))
  cfg$default$bounds[["42602"]] <- c(-10, 30)
  yaml::write_yaml(cfg, file.path(root, "config.yml"))
  expect_error(withr::with_dir(root, targets::tar_make(reporter = "silent")))
  expect_false(file.exists(file.path(root, "data", "warehouse.duckdb")))
})
