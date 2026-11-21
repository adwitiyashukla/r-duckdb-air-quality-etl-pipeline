ingest_one <- function(cfg, id) {
  cat <- source_catalog(cfg)
  row <- cat[cat$id == id, ]
  ingest_source(row, probe_sources(row, cfg), cfg)
}

test_that("hourly zips become typed parquet with lineage in the footer", {
  root <- withr::local_tempdir()
  fx <- build_fixture_source(file.path(root, "source"))
  cfg <- fixture_config(file.path(root, "data"), fx$dir)
  path <- ingest_one(cfg, "hourly_44201_2024")
  with_duck(cfg, function(con) {
    types <- duck_get(con, paste0("describe select * from read_parquet(", sql_path(path), ")"))
    expect_equal(types$column_type[types$column_name == "time_local"], "TIMESTAMP")
    expect_equal(types$column_type[types$column_name == "value"], "DOUBLE")
    expect_equal(types$column_type[types$column_name == "poc"], "INTEGER")
    expect_equal(parquet_rows(con, path), nrow(fixture_hourly()[["44201_2024"]]))
    kv <- parquet_kv(con, path)
    expect_equal(kv$sha256, secretbase::sha256(file = file.path(cfg$paths$data, "landing", "hourly_44201_2024.zip")))
    expect_equal(kv$rows_rejected, "0")
    expect_equal(kv$source_file, "hourly_44201_2024.zip")
  })
})

test_that("a renamed column is reported as layout drift instead of loading", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source")
  dir.create(src)
  df <- fixture_hourly()[["42602_2024"]]
  names(df)[names(df) == "Sample Measurement"] <- "Sample Value"
  write_epa_zip(src, "hourly_42602_2024", df)
  cfg <- fixture_config(file.path(root, "data"), src)
  expect_error(ingest_one(cfg, "hourly_42602_2024"), "expected hourly layout")
})

test_that("rows that fail type casts land in the rejects file", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source")
  dir.create(src)
  df <- fixture_hourly()[["42602_2024"]]
  df[["Sample Measurement"]][3] <- "not a number"
  df[["Time Local"]][5] <- "25:00"
  write_epa_zip(src, "hourly_42602_2024", df)
  cfg <- fixture_config(file.path(root, "data"), src)
  path <- ingest_one(cfg, "hourly_42602_2024")
  with_duck(cfg, function(con) {
    expect_equal(parquet_rows(con, path), nrow(df) - 2)
    expect_equal(parquet_rows(con, reject_path(cfg, list(id = "hourly_42602_2024"))), 2)
    expect_equal(parquet_kv(con, path)$rows_rejected, "2")
  })
})

test_that("the landing zip can be dropped after conversion", {
  root <- withr::local_tempdir()
  fx <- build_fixture_source(file.path(root, "source"))
  cfg <- fixture_config(file.path(root, "data"), fx$dir)
  cfg$landing$keep <- FALSE
  path <- ingest_one(cfg, "aqi_county_2024")
  expect_true(file.exists(path))
  expect_false(file.exists(file.path(cfg$paths$data, "landing", "daily_aqi_by_county_2024.zip")))
})
