plot_uri <- function(plot, width = 9, height = 4.8) {
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = width, height = height, units = "in", res = 144, type = "cairo", bg = "white")
  tryCatch(print(plot), finally = grDevices::dev.off())
  base64enc::dataURI(file = file, mime = "image/png")
}

html_table <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  cell <- function(tag, v, right) tag(class = if (right) "num", v)
  head <- htmltools::tags$tr(Map(function(n, r) cell(htmltools::tags$th, n, r), names(df), num))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(Map(function(v, r) {
      cell(htmltools::tags$td, if (r) format(v, big.mark = ",", scientific = FALSE) else as.character(v), r)
    }, df[i, , drop = TRUE], num))
  })
  htmltools::tags$table(htmltools::tags$thead(head), htmltools::tags$tbody(rows))
}

report_css <- paste0(
  "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;max-width:1000px;",
  "margin:32px auto;padding:0 16px;color:#1f2933;line-height:1.55;background:#fff}",
  "h1{font-size:30px;line-height:1.25;margin:0 0 8px}",
  "h2{font-size:21px;margin-top:40px;border-bottom:1px solid #e4e7eb;padding-bottom:6px}",
  ".lead{color:#3e4c59;font-size:17px;margin:0 0 8px}.muted{color:#616e7c;font-size:14px}",
  ".cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:12px;margin:22px 0}",
  ".card{background:#f5f7fa;border-radius:10px;padding:14px 16px}",
  ".card b{display:block;font-size:26px;color:#102a43}",
  "table{border-collapse:collapse;width:100%;font-size:14px;margin:8px 0}",
  "th,td{border-bottom:1px solid #e4e7eb;padding:7px 10px;text-align:left}",
  "th{background:#f5f7fa;font-weight:600}.num{text-align:right;font-variant-numeric:tabular-nums}",
  "img{max-width:100%;height:auto;display:block;margin:12px 0}",
  "footer{margin:48px 0 24px;color:#616e7c;font-size:13px;border-top:1px solid #e4e7eb;padding-top:12px}"
)

level_label <- function(level) {
  c(
    eighthour = "Ozone 8 hour averages", daily = "Daily summary per monitor",
    county = "County daily AQI", cbsa = "Metro area (CBSA) daily AQI"
  )[level]
}

match_sql <- function(by) {
  paste0(
    "select ", by, ", sum(n) filter (where status <> 'extra_in_rebuild')::double as epa_rows, ",
    "sum(n) filter (where status = 'match' or (level in ('county', 'cbsa') and status = 'aqi_match'))::double as same ",
    "from recon_summary group by all order by all"
  )
}

build_report <- function(cfg, warehouse) {
  path <- data_dir(cfg, "report", "index.html")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- open_warehouse(warehouse)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  q <- function(sql) DBI::dbGetQuery(con, sql)
  rows <- q("select sum(rows_read)::double as n from sources where kind = 'hourly'")$n
  monitors <- q("select count(distinct site_id || poc || parameter_code) as n from monitor_aqi")$n
  levels <- q(match_sql("level"))
  levels <- levels[match(c("eighthour", "daily", "county", "cbsa"), levels$level), ]
  rate <- function(level) sprintf("%.2f%%", 100 * levels$same[levels$level == level] / levels$epa_rows[levels$level == level])
  level_table <- data.frame(
    level = unname(level_label(levels$level)), epa_rows = levels$epa_rows, same_as_epa = levels$same,
    percent = sprintf("%.3f%%", 100 * levels$same / levels$epa_rows)
  )
  by_year <- q(match_sql("year, level"))
  by_year$rate <- 100 * by_year$same / by_year$epa_rows
  by_year$level <- factor(unname(level_label(by_year$level)), levels = unname(level_label(c("eighthour", "daily", "county", "cbsa"))))
  year_plot <- ggplot2::ggplot(by_year, ggplot2::aes(x = .data$year, y = .data$rate, colour = .data$level)) +
    ggplot2::geom_line(linewidth = 1, data = function(x) x[x$level %in% x$level[duplicated(x$level)], ]) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = unique(by_year$year)) +
    ggplot2::labs(x = NULL, y = "percent of EPA rows reproduced", colour = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
  reasons <- q(paste0(
    "select status, reason, count(*) as county_days from county_aqi where status not in ('match', 'aqi_match') ",
    "group by all order by county_days desc"
  ))
  daily_reasons <- q(paste0(
    "select reason, sum(n)::double as rows from recon_summary where level = 'daily' and status <> 'match' ",
    "group by all order by rows desc limit 6"
  ))
  worst <- q(paste0(
    "select date_local, count(*) filter (where aqi > 150) as unhealthy_counties from county_aqi ",
    "group by all order by unhealthy_counties desc, date_local limit 1"
  ))
  day <- worst$date_local[[1]]
  map <- q(paste0("select state_code, county_code, category from county_aqi where aqi is not null and date_local = '", format(day), "'"))
  shapes <- merge(county_shapes(), map, by = c("state_code", "county_code"), all.x = TRUE, sort = FALSE)
  shapes <- shapes[order(shapes$order), ]
  shapes$category <- factor(ifelse(is.na(shapes$category), "No monitor", shapes$category), levels = c(names(aqi_colors), "No monitor"))
  map_plot <- ggplot2::ggplot(shapes, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data$category)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.05) +
    ggplot2::geom_polygon(
      data = state_shapes(), ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = NA, colour = "grey30", linewidth = 0.2, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = c(aqi_colors, "No monitor" = "grey88"), drop = TRUE, name = NULL) +
    ggplot2::coord_quickmap() +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
  shift <- q(paste0(
    "select year, coalesce(sum(county_days) filter (where category_2012_breakpoints <> category_2024_breakpoints), 0)::double as moved, ",
    "sum(county_days)::double as total from pm25_breakpoint_shift group by all order by year"
  ))
  shift_plot <- ggplot2::ggplot(shift, ggplot2::aes(x = factor(.data$year), y = 100 * .data$moved / .data$total)) +
    ggplot2::geom_col(fill = "#3e7cb1") +
    ggplot2::labs(x = NULL, y = "percent of county-days moved up") +
    ggplot2::theme_minimal(base_size = 13)
  top <- q(paste0(
    "select any_value(state_name) as state, any_value(county_name) as county, ",
    "sum(unhealthy + very_unhealthy + hazardous)::integer as unhealthy_days, max(max_aqi) as worst_aqi ",
    "from county_year where state_code < '60' group by state_code, county_code order by unhealthy_days desc limit 12"
  ))
  checks <- q("select count(*) as steps, count(*) filter (where warn) as warned, count(*) filter (where stop) as failed from quality")
  years <- paste(cfg$years, collapse = " to ")
  card <- function(value, label) htmltools::div(class = "card", htmltools::tags$b(value), htmltools::span(class = "muted", label))
  body <- htmltools::tags$body(
    htmltools::h1("Rebuilding the EPA Air Quality Index from raw monitor readings"),
    htmltools::p(class = "lead", paste0(
      "Hourly EPA AirData files for ", years, ", rebuilt into daily AQI per monitor, county and metro area with an R targets ",
      "pipeline on DuckDB, then checked row by row against the daily AQI files EPA publishes."
    )),
    htmltools::div(
      class = "cards",
      card(format(rows, big.mark = ",", scientific = FALSE), "hourly readings"),
      card(format(monitors, big.mark = ","), "monitors"),
      card(rate("county"), "county-days with the same AQI as EPA"),
      card(rate("eighthour"), "ozone 8 hour averages equal to EPA's")
    ),
    htmltools::h2("How close the rebuild gets"),
    html_table(level_table),
    htmltools::tags$img(src = plot_uri(year_plot, height = 4.6), alt = "match rate by year"),
    htmltools::p(class = "muted", paste(
      "Most of the daily rows that differ are PM2.5 monitors where EPA changed the hourly data after it built the daily file,",
      "or Teledyne T640 monitors that EPA moved to corrected method codes 736 and 738."
    )),
    html_table(daily_reasons),
    htmltools::h2("Why the remaining county-days differ"),
    html_table(reasons),
    htmltools::h2(paste("Worst day in the data:", format(day))),
    htmltools::p(class = "muted", sprintf("%d counties were above AQI 150 that day.", worst$unhealthy_counties[[1]])),
    htmltools::tags$img(src = plot_uri(map_plot, height = 5.6), alt = "county AQI map"),
    htmltools::h2("What the 2024 PM2.5 breakpoints did to the history"),
    htmltools::p(class = "muted", paste(
      "EPA tightened the PM2.5 AQI breakpoints in May 2024 and AirData now applies them to every past year.",
      "This is the share of county-days that land in a worse category than they would under the 2012 breakpoints."
    )),
    htmltools::tags$img(src = plot_uri(shift_plot, height = 4), alt = "PM2.5 breakpoint shift"),
    htmltools::h2("US counties with the most unhealthy days"),
    html_table(top),
    htmltools::h2("Data contracts"),
    htmltools::p(sprintf(
      "%d pointblank and gate checks ran on this build. %d raised a warning and %d failed.",
      checks$steps, checks$warned, checks$failed
    )),
    htmltools::tags$footer(paste("Data from EPA AirData. Built", format(Sys.Date())))
  )
  head <- paste0(
    "<head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>EPA AQI rebuild</title><style>", report_css, "</style></head>"
  )
  writeLines(c("<!DOCTYPE html>", "<html lang=\"en\">", head, as.character(body), "</html>"), path, useBytes = TRUE)
  path
}
