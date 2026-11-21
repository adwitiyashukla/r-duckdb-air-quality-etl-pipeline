aqi_colors <- c(
  "Good" = "#00E400", "Moderate" = "#FFFF00", "Unhealthy for Sensitive Groups" = "#FF7E00",
  "Unhealthy" = "#FF0000", "Very Unhealthy" = "#8F3F97", "Hazardous" = "#7E0023"
)

county_shapes <- function() {
  shapes <- ggplot2::map_data("county")
  fips <- maps::county.fips
  key <- paste(shapes$region, shapes$subregion, sep = ",")
  base <- sub(":.*$", "", fips$polyname)
  code <- fips$fips[match(key, fips$polyname)]
  code[is.na(code)] <- fips$fips[match(key[is.na(code)], base)]
  shapes$state_code <- sprintf("%02d", code %/% 1000)
  shapes$county_code <- sprintf("%03d", code %% 1000)
  shapes[!is.na(code), ]
}

state_shapes <- function() {
  ggplot2::map_data("state")
}

dt_table <- function(data, page = 10) {
  DT::datatable(data, rownames = FALSE, options = list(pageLength = page, scrollX = TRUE))
}

dashboard_ui <- function() {
  bslib::page_navbar(
    title = "EPA AQI rebuild",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    bslib::nav_panel(
      "Overview",
      bslib::layout_columns(
        bslib::value_box("County-days rebuilt", shiny::textOutput("vb_days")),
        bslib::value_box("County AQI equal to EPA", shiny::textOutput("vb_match")),
        bslib::value_box("Hourly readings ingested", shiny::textOutput("vb_rows")),
        bslib::value_box("Monitors", shiny::textOutput("vb_monitors"))
      ),
      bslib::layout_columns(
        bslib::card(bslib::card_header("Match rate by year"), shiny::plotOutput("match_plot", height = 320)),
        bslib::card(bslib::card_header("Why the other county-days differ"), DT::DTOutput("reason_table"))
      )
    ),
    bslib::nav_panel(
      "Map",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::dateInput("map_date", "Day", value = NULL),
          shiny::helpText("Counties without a monitor stay grey.")
        ),
        bslib::card(shiny::plotOutput("map_plot", height = 520)),
        bslib::card(bslib::card_header("Worst counties that day"), DT::DTOutput("worst_table"))
      )
    ),
    bslib::nav_panel(
      "County",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput("state", "State", choices = NULL),
          shiny::selectInput("county", "County", choices = NULL),
          shiny::sliderInput("years", "Years", min = 2016, max = 2025, value = c(2024, 2025), sep = "", step = 1)
        ),
        bslib::card(
          bslib::card_header("Daily AQI, rebuilt (line) and EPA where it differs (dots)"),
          shiny::plotOutput("series_plot", height = 320)
        ),
        bslib::card(bslib::card_header("Days per category"), shiny::plotOutput("category_plot", height = 300))
      )
    ),
    bslib::nav_panel(
      "Reconciliation",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::selectInput("level", "Level", choices = c("county", "cbsa", "daily", "eighthour")),
          shiny::selectInput("recon_year", "Year", choices = NULL)
        ),
        bslib::card(bslib::card_header("Status counts"), DT::DTOutput("recon_table")),
        bslib::card(bslib::card_header("Rows that differ"), DT::DTOutput("mismatch_table"))
      )
    ),
    bslib::nav_panel("Data quality", bslib::card(DT::DTOutput("quality_table"))),
    bslib::nav_panel("Sources", bslib::card(DT::DTOutput("sources_table")))
  )
}

dashboard_server <- function(con) {
  shapes <- county_shapes()
  states <- state_shapes()
  function(input, output, session) {
    q <- function(sql, params = list()) query_rows(con, sql, params)
    totals <- q(paste0(
      "select count(*) as days, count(*) filter (where status in ('match', 'aqi_match')) as matched, ",
      "min(date_local)::varchar as first_day, max(date_local)::varchar as last_day from county_aqi where aqi is not null"
    ))
    output$vb_days <- shiny::renderText(format(totals$days, big.mark = ","))
    output$vb_match <- shiny::renderText(sprintf("%.2f%%", 100 * totals$matched / totals$days))
    output$vb_rows <- shiny::renderText(format(
      q("select sum(rows_read)::double as n from sources where kind = 'hourly' and role = 'core'")$n,
      big.mark = ",", scientific = FALSE
    ))
    output$vb_monitors <- shiny::renderText(format(
      q("select count(distinct site_id || poc || parameter_code) as n from monitor_aqi")$n,
      big.mark = ","
    ))
    output$match_plot <- shiny::renderPlot({
      d <- q(paste0(
        "select year, level, sum(n) filter (where status in ('match', 'aqi_match'))::double / sum(n) as rate ",
        "from recon_summary group by all order by year"
      ))
      ggplot2::ggplot(d, ggplot2::aes(x = year, y = 100 * rate, colour = level)) +
        ggplot2::geom_line(linewidth = 1, data = function(x) x[x$level %in% x$level[duplicated(x$level)], ]) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_continuous(breaks = unique(d$year)) +
        ggplot2::labs(x = NULL, y = "percent of rows that match EPA", colour = NULL) +
        ggplot2::theme_minimal(base_size = 13)
    })
    output$reason_table <- DT::renderDT(dt_table(q(paste0(
      "select status, reason, count(*) as county_days from county_aqi where status not in ('match', 'aqi_match') ",
      "group by all order by county_days desc"
    ))))

    shiny::updateDateInput(session, "map_date", value = totals$last_day, min = totals$first_day, max = totals$last_day)
    map_data <- shiny::reactive({
      shiny::req(input$map_date)
      q("select state_code, county_code, aqi, category from county_aqi where date_local = ?::date and aqi is not null",
        list(format(input$map_date)))
    })
    output$map_plot <- shiny::renderPlot({
      d <- merge(shapes, map_data(), by = c("state_code", "county_code"), all.x = TRUE, sort = FALSE)
      d <- d[order(d$order), ]
      d$category <- factor(d$category, levels = names(aqi_colors))
      ggplot2::ggplot(d, ggplot2::aes(x = long, y = lat, group = group, fill = category)) +
        ggplot2::geom_polygon(colour = "white", linewidth = 0.05) +
        ggplot2::geom_polygon(
          data = states, ggplot2::aes(x = long, y = lat, group = group),
          fill = NA, colour = "grey30", linewidth = 0.2, inherit.aes = FALSE
        ) +
        ggplot2::scale_fill_manual(values = aqi_colors, na.value = "grey88", drop = FALSE, name = NULL) +
        ggplot2::coord_quickmap() +
        ggplot2::theme_void(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })
    output$worst_table <- DT::renderDT({
      shiny::req(input$map_date)
      dt_table(api_worst(con, format(input$map_date), 15L))
    })

    state_list <- q(paste0(
      "select state_code, any_value(state_name) as state_name from county_aqi ",
      "where state_name is not null group by all order by state_name"
    ))
    shiny::updateSelectInput(session, "state", choices = stats::setNames(state_list$state_code, state_list$state_name), selected = "39")
    shiny::observeEvent(input$state, {
      counties <- api_counties(con, input$state)
      shiny::updateSelectInput(session, "county", choices = stats::setNames(counties$county_code, counties$county_name))
    })
    years <- q("select distinct year from county_year order by year")$year
    shiny::updateSliderInput(session, "years", min = min(years), max = max(years), value = c(max(min(years), max(years) - 1), max(years)))
    output$series_plot <- shiny::renderPlot({
      shiny::req(input$state, input$county)
      d <- api_county_series(con, input$state, input$county, sprintf("%d-01-01", input$years[[1]]), sprintf("%d-12-31", input$years[[2]]))
      shiny::validate(shiny::need(nrow(d) > 0, "No data for this county in these years"))
      d$date_local <- as.Date(d$date_local)
      off <- d[!is.na(d$epa_aqi) & (is.na(d$aqi) | d$aqi != d$epa_aqi), ]
      ggplot2::ggplot(d, ggplot2::aes(x = date_local, y = aqi)) +
        ggplot2::geom_line(colour = "grey35", linewidth = 0.3) +
        ggplot2::geom_point(data = off, ggplot2::aes(y = epa_aqi), colour = "#d7301f", size = 2) +
        ggplot2::geom_hline(yintercept = c(50, 100, 150), linetype = "dashed", colour = "grey70") +
        ggplot2::labs(x = NULL, y = "AQI") +
        ggplot2::theme_minimal(base_size = 13)
    })
    output$category_plot <- shiny::renderPlot({
      shiny::req(input$state, input$county)
      d <- q(paste0(
        "select year, good, moderate, usg, unhealthy, very_unhealthy, hazardous from county_year ",
        "where state_code = ? and county_code = ? order by year"
      ), list(input$state, input$county))
      shiny::validate(shiny::need(nrow(d) > 0, "No data for this county"))
      long <- data.frame(
        year = rep(d$year, 6),
        category = factor(rep(names(aqi_colors), each = nrow(d)), levels = rev(names(aqi_colors))),
        days = c(d$good, d$moderate, d$usg, d$unhealthy, d$very_unhealthy, d$hazardous)
      )
      ggplot2::ggplot(long, ggplot2::aes(x = factor(year), y = days, fill = category)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = aqi_colors, name = NULL) +
        ggplot2::labs(x = NULL, y = "days") +
        ggplot2::theme_minimal(base_size = 13)
    })

    recon_years <- q("select distinct year from recon_summary order by year")$year
    shiny::updateSelectInput(session, "recon_year", choices = recon_years, selected = max(recon_years))
    output$recon_table <- DT::renderDT(dt_table(q(
      "select year, status, reason, sum(n)::bigint as n from recon_summary where level = ? group by all order by year, n desc",
      list(input$level)
    )))
    output$mismatch_table <- DT::renderDT({
      shiny::req(input$recon_year)
      dt_table(api_mismatches(con, input$level, as.integer(input$recon_year), 500L))
    })
    output$quality_table <- DT::renderDT(dt_table(q(
      "select contract, year, step, assertion, columns, label, n, n_failed, warn, stop from quality order by contract, year, step"
    ), page = 25))
    output$sources_table <- DT::renderDT(dt_table(q(
      "select source_file, role, bytes, rows_read, rows_rejected, last_modified, sha256 from sources order by source_file"
    ), page = 25))
  }
}

run_dashboard <- function(warehouse = "data/warehouse.duckdb", host = "127.0.0.1", port = 8080) {
  con <- open_warehouse(warehouse)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  app <- shiny::shinyApp(dashboard_ui(), dashboard_server(con))
  shiny::runApp(app, host = host, port = port, launch.browser = FALSE)
}
