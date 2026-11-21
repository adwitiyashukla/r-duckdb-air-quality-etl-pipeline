test_that("the api handlers answer from the warehouse", {
  con <- open_warehouse(fixture_warehouse())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  health <- api_health(con)
  expect_equal(health$status, "ok")
  expect_equal(health$years, "2024-2024")
  counties <- api_counties(con, "39")
  expect_setequal(counties$county_name, c("Butler", "Hamilton"))
  series <- api_county_series(con, "39", "017", "2024-01-01", "2024-01-31")
  expect_true(all(c("aqi", "epa_aqi", "status") %in% names(series)))
  expect_equal(series$status[series$date_local == "2024-01-03"], "aqi_differs")
  worst <- api_worst(con, "2024-01-03", 5L)
  expect_equal(worst$county_name[1], "Hamilton")
  expect_true(all(c("match", "aqi_differs") %in% api_reconciliation(con, "county")$status))
  expect_equal(nrow(api_mismatches(con, "county", 2024L, 10L)), 1L)
})

test_that("bad query parameters get a 400 with a message", {
  res <- new.env()
  expect_match(bad_request(res, "nope")$error, "nope")
  expect_equal(res$status, 400L)
  expect_true(valid_code("06", 2))
  expect_false(valid_code("6", 2))
  expect_false(valid_code("06; drop table x", 2))
  expect_true(valid_date("2024-02-29"))
  expect_false(valid_date("2023-02-29"))
  expect_false(valid_date("2024/01/01"))
})

test_that("the api serves http on a real port", {
  warehouse <- fixture_warehouse()
  port <- 18000L + sample.int(1000, 1)
  proc <- callr::r_bg(function(root, warehouse, port) {
    for (f in list.files(file.path(root, "R"), full.names = TRUE)) sys.source(f, envir = globalenv())
    run_api(warehouse, port = port)
  }, args = list(root = project_root(), warehouse = warehouse, port = port))
  on.exit(proc$kill(), add = TRUE)
  url <- sprintf("http://127.0.0.1:%d", port)
  ok <- FALSE
  for (i in 1:60) {
    Sys.sleep(0.5)
    resp <- tryCatch(httr2::req_perform(httr2::request(paste0(url, "/health"))), error = function(e) NULL)
    if (!is.null(resp)) {
      ok <- TRUE
      break
    }
  }
  expect_true(ok)
  body <- httr2::resp_body_json(resp)
  expect_equal(body$status, "ok")
  bad <- httr2::request(paste0(url, "/aqi/county?state=x")) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(bad), 400L)
  rows <- httr2::request(paste0(url, "/aqi/county?state=39&county=061&from=2024-01-01&to=2024-01-31")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_gt(length(rows), 2)
})
