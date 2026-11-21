quality_path <- function(cfg, contract, year) {
  data_dir(cfg, "gold", "quality", paste0(contract, "_", year, ".parquet"))
}

parquet_tbl <- function(con, files, extra = "") {
  select <- if (nzchar(extra)) paste0("*, ", extra) else "*"
  dplyr::tbl(con, dplyr::sql(paste0("select ", select, " from read_parquet(", sql_paths(files), ")")))
}

agent_results <- function(agent, contract, year) {
  x <- pointblank::get_agent_x_list(agent)
  cols <- vapply(x$columns, function(c) paste(unlist(c), collapse = ","), character(1))
  tibble::tibble(
    contract = contract,
    year = as.integer(year),
    step = seq_along(x$type),
    assertion = x$type,
    columns = cols,
    label = vapply(x$briefs, function(b) if (is.null(b) || is.na(b)) "" else as.character(b), character(1)),
    n = as.numeric(x$n),
    n_failed = as.numeric(x$n_failed),
    f_failed = as.numeric(x$f_failed),
    warn = as.logical(x$warn),
    stop = as.logical(x$stop),
    eval_error = as.logical(x$eval_error)
  )
}

finish_contract <- function(cfg, results, path) {
  results$warn[is.na(results$warn)] <- FALSE
  results$stop[is.na(results$stop)] <- FALSE
  results$eval_error[is.na(results$eval_error)] <- FALSE
  with_duck(cfg, function(con) {
    duckdb::duckdb_register(con, "results", results)
    write_parquet(con, "select * from results", path)
  })
  bad <- results[results$stop | results$eval_error, ]
  if (nrow(bad) > 0) {
    lines <- sprintf(
      "%s step %d %s on %s: %s of %s failed",
      bad$contract, bad$step, bad$assertion, bad$columns, bad$n_failed, bad$n
    )
    cli::cli_abort(c("Data contract failed", stats::setNames(lines, rep("x", length(lines)))))
  }
  path
}

contract_hourly <- function(cfg, files, year) {
  path <- quality_path(cfg, "bronze_hourly", year)
  bounds <- cfg$bounds
  p <- pollutants()
  results <- with_duck(cfg, function(con) {
    tbl <- parquet_tbl(con, files, "date_diff('hour', time_local, time_gmt) as gmt_offset_hours, year(time_local) as obs_year")
    agent <- pointblank::create_agent(
      tbl = tbl, tbl_name = paste0("bronze_hourly_", year), label = paste("bronze hourly", year),
      actions = pointblank::action_levels(warn_at = 1, stop_at = cfg$quality$max_out_of_bounds_rate)
    ) |>
      pointblank::col_vals_not_null(
        columns = c("site_id", "parameter_code", "poc", "time_local", "time_gmt", "value"),
        actions = pointblank::action_levels(stop_at = 1)
      ) |>
      pointblank::rows_distinct(
        columns = c("site_id", "parameter_code", "poc", "time_local"),
        actions = pointblank::action_levels(warn_at = 1, stop_at = 1)
      ) |>
      pointblank::col_vals_equal(columns = "obs_year", value = year, actions = pointblank::action_levels(stop_at = 1)) |>
      pointblank::col_vals_between(columns = "gmt_offset_hours", left = -12, right = 12, actions = pointblank::action_levels(stop_at = 1))
    for (code in p$parameter_code) {
      only <- local({
        k <- code
        function(x) dplyr::filter(x, .data$parameter_code == !!k)
      })
      agent <- agent |>
        pointblank::col_vals_between(
          columns = "value", left = bounds[[code]][[1]], right = bounds[[code]][[2]],
          preconditions = only, label = paste("value bounds", code)
        ) |>
        pointblank::col_vals_in_set(
          columns = "units", set = p$units[p$parameter_code == code],
          preconditions = only, label = paste("units", code), actions = pointblank::action_levels(stop_at = 1)
        )
    }
    agent_results(pointblank::interrogate(agent, progress = FALSE), "bronze_hourly", year)
  })
  finish_contract(cfg, results, path)
}

contract_rejects <- function(cfg, files, year) {
  path <- quality_path(cfg, "bronze_rejects", year)
  results <- with_duck(cfg, function(con) {
    kv <- lapply(files, function(f) parquet_kv(con, f))
    read <- vapply(kv, function(k) as.numeric(k$rows_read), numeric(1))
    rejected <- vapply(kv, function(k) as.numeric(k$rows_rejected), numeric(1))
    rate <- ifelse(read > 0, rejected / read, 0)
    tibble::tibble(
      contract = "bronze_rejects", year = as.integer(year), step = seq_along(files),
      assertion = "reject_rate_below", columns = vapply(kv, function(k) k$source_file, character(1)),
      label = sprintf("rejected %s of %s rows", rejected, read), n = read, n_failed = rejected,
      f_failed = rate, warn = rejected > 0, stop = rate > cfg$quality$max_reject_rate, eval_error = FALSE
    )
  })
  finish_contract(cfg, results, path)
}

contract_area <- function(cfg, files, year) {
  path <- quality_path(cfg, "gold_area", year)
  results <- with_duck(cfg, function(con) {
    out <- list()
    for (level in names(files)) {
      keys <- if (level == "county") c("state_code", "county_code", "date_local") else c("cbsa_code", "date_local")
      tbl <- parquet_tbl(con, files[[level]])
      agent <- pointblank::create_agent(
        tbl = tbl, tbl_name = paste0(level, "_aqi_", year), label = paste(level, "aqi", year),
        actions = pointblank::action_levels(stop_at = 1)
      ) |>
        pointblank::col_vals_not_null(columns = tidyselect::all_of(c(keys, "aqi", "category", "defining_parameter", "defining_site"))) |>
        pointblank::rows_distinct(columns = tidyselect::all_of(keys)) |>
        pointblank::col_vals_gte(columns = "aqi", value = 0) |>
        pointblank::col_vals_gte(columns = "n_sites", value = 1) |>
        pointblank::col_vals_in_set(columns = "defining_parameter", set = c("Ozone", "PM2.5", "PM10", "CO", "NO2")) |>
        pointblank::col_vals_in_set(
          columns = "category",
          set = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
        )
      res <- agent_results(pointblank::interrogate(agent, progress = FALSE), paste0("gold_", level), year)
      out[[level]] <- res
    }
    do.call(rbind, out)
  })
  finish_contract(cfg, results, path)
}

contract_reconciliation <- function(cfg, summaries, year) {
  path <- quality_path(cfg, "reconciliation", year)
  gates <- c(
    eighthour = cfg$quality$eighthour_match_min, daily = cfg$quality$daily_match_min,
    county = cfg$quality$county_match_min, cbsa = cfg$quality$cbsa_match_min
  )
  results <- with_duck(cfg, function(con) {
    s <- duck_get(con, fill(paste0(
      "select level, sum(n) filter (where status = 'match' or (level in ('county', 'cbsa') and status = 'aqi_match'))::double as matched, ",
      "sum(n) filter (where status <> 'extra_in_rebuild')::double as total from read_parquet(<<sql_paths(summaries)>>) group by all"
    )))
    s <- s[match(names(gates), s$level), ]
    rate <- s$matched / s$total
    tibble::tibble(
      contract = "reconciliation", year = as.integer(year), step = seq_along(gates), assertion = "match_rate_at_least",
      columns = names(gates), label = sprintf("matched %.0f of %.0f, gate %.4f", s$matched, s$total, gates),
      n = s$total, n_failed = s$total - s$matched, f_failed = 1 - rate,
      warn = rate < 1, stop = is.na(rate) | rate < gates, eval_error = FALSE
    )
  })
  finish_contract(cfg, results, path)
}
