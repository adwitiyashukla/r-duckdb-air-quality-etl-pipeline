test_that("county shapes carry state and county codes", {
  shapes <- county_shapes()
  expect_true(all(c("state_code", "county_code") %in% names(shapes)))
  expect_true(any(shapes$state_code == "39" & shapes$county_code == "061"))
  expect_gt(length(unique(paste(shapes$state_code, shapes$county_code))), 3000)
})

test_that("the dashboard server renders its main outputs", {
  con <- open_warehouse(fixture_warehouse())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  shiny::testServer(dashboard_server(con), {
    session$setInputs(
      map_date = as.Date("2024-01-03"), state = "39", county = "061", years = c(2024, 2024),
      level = "county", recon_year = "2024"
    )
    expect_match(output$vb_days, "[0-9]")
    expect_match(output$vb_match, "%")
    expect_true(is.list(output$map_plot))
    expect_true(is.list(output$series_plot))
    expect_true(is.list(output$category_plot))
    expect_match(output$mismatch_table, "epa_site")
    expect_no_match(output$mismatch_table, "mine_obs")
  })
})
