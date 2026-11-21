silver_path <- function(cfg, layer, parameter_code, year) {
  data_dir(cfg, "silver", layer, parameter_code, paste0(year, ".parquet"))
}

pollutant_row <- function(code) {
  p <- pollutants()
  p[p$parameter_code == code, ]
}

stamp_window <- function(year, from_offset_hours, to_offset_hours) {
  start <- as.POSIXct(sprintf("%d-01-01 00:00:00", year), tz = "UTC") + from_offset_hours * 3600
  end <- as.POSIXct(sprintf("%d-01-01 00:00:00", year + 1), tz = "UTC") + to_offset_hours * 3600
  c(format(start, "%Y-%m-%d %H:%M:%S", tz = "UTC"), format(end, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

load_hourly <- function(con, files, code, window) {
  duck_exec(con, fill(paste0(
    "create or replace temp table h as select site_id, poc, time_local as ts, value, method_code, last_change ",
    "from read_parquet(<<sql_paths(files)>>) where parameter_code = <<sql_lit(code)>> ",
    "and time_local >= timestamp <<sql_lit(window[[1]])>> and time_local < timestamp <<sql_lit(window[[2]])>>"
  )))
}

hourly_grid <- function(con, lead_hours, lag_hours) {
  duck_exec(con, fill(paste0(
    "create or replace temp table g as ",
    "with span as (select site_id, poc, min(ts) as t0, max(ts) as t1 from h group by all) ",
    "select site_id, poc, unnest(range(t0 - interval <<lead_hours>> hour, t1 + interval <<lag_hours + 1>> hour, interval 1 hour)) as ts ",
    "from span"
  )))
}

one_hour_stats <- function(code, year, with_aqi, breakpoints) {
  p <- pollutant_row(code)
  aqi <- if (with_aqi) aqi_sql("max_value", p$breakpoints, breakpoints, p$decimals) else "null::integer"
  fill(paste0(
    "select site_id, <<sql_lit(code)>> as parameter_code, poc, '1 HOUR' as sample_duration, date_local, obs_count, ",
    "mean, max_value, max_hour, <<aqi>> as aqi, hourly_last_change, method_code from (",
    "select site_id, poc, cast(ts as date) as date_local, count(value)::integer as obs_count, ",
    "round(avg(value), 6) as mean, max(value) as max_value, ",
    "first(hour(ts) order by value desc, ts)::integer as max_hour, max(last_change) as hourly_last_change, ",
    "max(method_code) as method_code ",
    "from h where year(ts) = <<year>> group by all)"
  ))
}

rebuild_ozone <- function(cfg, hourly, year, breakpoints) {
  out <- c(
    daily = silver_path(cfg, "daily", "44201", year),
    aqi = silver_path(cfg, "aqi", "44201", year),
    eighthour = silver_path(cfg, "eighthour", "44201", year)
  )
  p <- pollutant_row("44201")
  with_duck(cfg, function(con) {
    define_macros(con)
    load_hourly(con, hourly, "44201", stamp_window(year, 0, 0))
    hourly_grid(con, 2, 0)
    duck_exec(con, fill(paste0(
      "create temp table e as select site_id, poc, ts as begin_local, n::integer as obs_count, ",
      "trunc_to(s / n, 3) as mean8, last_change, method_code from (",
      "select g.site_id, g.poc, g.ts, count(h.value) over w as n, sum(h.value) over w as s, ",
      "max(h.last_change) over w as last_change, max(h.method_code) over w as method_code ",
      "from g left join h using (site_id, poc, ts) ",
      "window w as (partition by g.site_id, g.poc order by g.ts rows between current row and 7 following)) ",
      "where n >= 6 and year(ts) = <<year>>"
    )))
    write_parquet(con, "select site_id, poc, begin_local, obs_count, mean8 from e order by site_id, poc, begin_local", out[["eighthour"]])
    write_parquet(con, fill(paste0(
      "select site_id, '44201' as parameter_code, poc, '8-HR RUN AVG BEGIN HOUR' as sample_duration, date_local, ",
      "obs_count, mean, max_value, max_hour, <<aqi_sql('max_value', p$breakpoints, breakpoints, p$decimals)>> as aqi, ",
      "hourly_last_change, method_code from (select site_id, poc, cast(begin_local as date) as date_local, ",
      "count(*)::integer as obs_count, round(avg(mean8), 6) as mean, max(mean8) as max_value, ",
      "first(hour(begin_local) order by mean8 desc, begin_local)::integer as max_hour, ",
      "max(last_change) as hourly_last_change, max(method_code) as method_code ",
      "from e where hour(begin_local) >= 7 group by all)"
    )), out[["daily"]])
    write_parquet(con, fill(paste0(
      "select site_id, '44201' as parameter_code, poc, date_local, 'Ozone' as aqi_parameter, ",
      "'8-HR RUN AVG BEGIN HOUR' as sample_duration, concentration, ",
      "<<aqi_sql('concentration', p$breakpoints, breakpoints, p$decimals)>> as aqi from (",
      "select site_id, poc, date_local, case when n24 >= 18 then mx24 when n17 >= 13 then mx17 end as concentration from (",
      "select site_id, poc, cast(begin_local as date) as date_local, count(*) as n24, max(mean8) as mx24, ",
      "count(*) filter (where hour(begin_local) >= 7) as n17, max(mean8) filter (where hour(begin_local) >= 7) as mx17 ",
      "from e group by all)) where concentration is not null"
    )), out[["aqi"]])
  })
  out
}

rebuild_co <- function(cfg, hourly, year, breakpoints) {
  out <- c(daily = silver_path(cfg, "daily", "42101", year), aqi = silver_path(cfg, "aqi", "42101", year))
  p <- pollutant_row("42101")
  with_duck(cfg, function(con) {
    define_macros(con)
    load_hourly(con, hourly, "42101", stamp_window(year, 0, 0))
    hourly_grid(con, 0, 2)
    duck_exec(con, fill(paste0(
      "create temp table e as select site_id, poc, ts as end_local, n::integer as obs_count, ",
      "round_half_up(s / n, 1) as mean8, last_change, method_code from (",
      "select g.site_id, g.poc, g.ts, count(h.value) over w as n, sum(h.value) over w as s, ",
      "max(h.last_change) over w as last_change, max(h.method_code) over w as method_code ",
      "from g left join h using (site_id, poc, ts) ",
      "window w as (partition by g.site_id, g.poc order by g.ts rows between 7 preceding and current row)) ",
      "where n >= 6 and year(ts) = <<year>>"
    )))
    write_parquet(con, fill(paste0(
      "select site_id, '42101' as parameter_code, poc, '8-HR RUN AVG END HOUR' as sample_duration, date_local, ",
      "obs_count, mean, max_value, max_hour, <<aqi_sql('max_value', p$breakpoints, breakpoints, p$decimals)>> as aqi, ",
      "hourly_last_change, method_code from (select site_id, poc, cast(end_local as date) as date_local, ",
      "count(*)::integer as obs_count, round(avg(mean8), 6) as mean, max(mean8) as max_value, ",
      "first(hour(end_local) order by mean8 desc, end_local)::integer as max_hour, ",
      "max(last_change) as hourly_last_change, max(method_code) as method_code ",
      "from e group by all) union all ", one_hour_stats("42101", year, FALSE, breakpoints)
    )), out[["daily"]])
    write_parquet(con, fill(paste0(
      "select site_id, '42101' as parameter_code, poc, date_local, 'CO' as aqi_parameter, ",
      "'8-HR RUN AVG END HOUR' as sample_duration, concentration, ",
      "<<aqi_sql('concentration', p$breakpoints, breakpoints, p$decimals)>> as aqi from (",
      "select site_id, poc, cast(end_local as date) as date_local, max(mean8) as concentration from e group by all)"
    )), out[["aqi"]])
  })
  out
}

rebuild_gas <- function(cfg, hourly, code, year, breakpoints) {
  out <- c(daily = silver_path(cfg, "daily", code, year), aqi = silver_path(cfg, "aqi", code, year))
  p <- pollutant_row(code)
  limit <- area_exceedance(code)
  with_duck(cfg, function(con) {
    define_macros(con)
    load_hourly(con, hourly, code, stamp_window(year, 0, 0))
    duck_exec(con, paste0("create temp table d as ", one_hour_stats(code, year, TRUE, breakpoints)))
    write_parquet(con, "select * from d", out[["daily"]])
    keep <- if (p$in_area_aqi) fill("obs_count >= 18 or max_value > <<limit>>") else "false"
    write_parquet(con, fill(paste0(
      "select site_id, parameter_code, poc, date_local, <<sql_lit(p$aqi_parameter)>> as aqi_parameter, ",
      "sample_duration, max_value as concentration, aqi from d where <<keep>>"
    )), out[["aqi"]])
  })
  out
}

area_exceedance <- function(code) {
  switch(code,
    "42602" = 100,
    "42401" = 75,
    "88101" = 35,
    "88502" = 35,
    "81102" = 150,
    NA_real_
  )
}

rebuild_pm <- function(cfg, hourly, daily, code, year, breakpoints) {
  out <- c(daily = silver_path(cfg, "daily", code, year), aqi = silver_path(cfg, "aqi", code, year))
  p <- pollutant_row(code)
  limit <- area_exceedance(code)
  block <- if (code == "81102") {
    fill("case when n >= 18 then trunc_to(a, 0) when trunc_to(s / 24, 0) > <<limit>> then trunc_to(s / 24, 0) end")
  } else {
    fill("case when n >= 18 then trunc_to(a, 1) when trunc_to(a, 1) > <<limit>> then trunc_to(a, 1) end")
  }
  with_duck(cfg, function(con) {
    define_macros(con)
    load_hourly(con, hourly, code, stamp_window(year, 0, 0))
    duck_exec(con, fill(paste0(
      "create temp table b as select site_id, poc, date_local, <<block>> as value, hourly_last_change, method_code from (",
      "select site_id, poc, cast(ts as date) as date_local, count(value) as n, avg(value) as a, sum(value) as s, ",
      "max(last_change) as hourly_last_change, max(method_code) as method_code from h where year(ts) = <<year>> group by all)"
    )))
    duck_exec(con, fill(paste0(
      "create temp table f as select site_id, poc, date_local, max(mean) as value, max(last_change) as last_change, ",
      "max(method_code) as method_code ",
      "from read_parquet(<<sql_path(daily)>>) where parameter_code = <<sql_lit(code)>> and sample_duration = '24 HOUR' ",
      "and event_type in ('None', 'Included') and mean is not null group by all"
    )))
    aqi <- aqi_sql("value", p$breakpoints, breakpoints, p$decimals)
    hourly_stats <- if (code == "81102") "" else paste(one_hour_stats(code, year, FALSE, breakpoints), "union all ")
    write_parquet(con, fill(paste0(
      hourly_stats,
      "select site_id, <<sql_lit(code)>> as parameter_code, poc, '24-HR BLK AVG' as sample_duration, date_local, ",
      "1 as obs_count, value as mean, value as max_value, 0 as max_hour, <<aqi>> as aqi, hourly_last_change, method_code ",
      "from b where value is not null union all ",
      "select site_id, <<sql_lit(code)>>, poc, '24 HOUR', date_local, 1, value, value, 0, <<aqi>>, last_change, method_code from f"
    )), out[["daily"]])
    write_parquet(con, fill(paste0(
      "select site_id, <<sql_lit(code)>> as parameter_code, poc, date_local, <<sql_lit(p$aqi_parameter)>> as aqi_parameter, ",
      "sample_duration, value as concentration, <<aqi>> as aqi from (",
      "select site_id, poc, date_local, '24-HR BLK AVG' as sample_duration, value from b where value is not null ",
      "union all select site_id, poc, date_local, '24 HOUR', value from f)"
    )), out[["aqi"]])
  })
  out
}

rebuild_pollutant <- function(cfg, code, year, hourly, daily, breakpoints) {
  switch(code,
    "44201" = rebuild_ozone(cfg, hourly, year, breakpoints),
    "42101" = rebuild_co(cfg, hourly, year, breakpoints),
    "42401" = ,
    "42602" = rebuild_gas(cfg, hourly, code, year, breakpoints),
    "88101" = ,
    "88502" = ,
    "81102" = rebuild_pm(cfg, hourly, daily, code, year, breakpoints),
    cli::cli_abort("No rebuild rule for parameter {code}")
  )
}
