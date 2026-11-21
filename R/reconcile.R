recon_paths <- function(cfg, level, year) {
  c(
    summary = data_dir(cfg, "gold", "recon", level, paste0(year, "_summary.parquet")),
    mismatch = data_dir(cfg, "gold", "recon", level, paste0(year, "_mismatch.parquet"))
  )
}

write_recon <- function(con, level, year, out) {
  write_parquet(con, fill(paste0(
    "select <<sql_lit(level)>> as level, <<year>>::integer as year, parameter_code, sample_duration, status, reason, ",
    "count(*)::bigint as n from j group by all order by all"
  )), out[["summary"]])
  write_parquet(con, "select * from j where status <> 'match'", out[["mismatch"]])
  out
}

reconcile_eighthour <- function(cfg, mine, epa, year) {
  out <- recon_paths(cfg, "eighthour", year)
  with_duck(cfg, function(con) {
    duck_exec(con, fill(paste0(
      "create temp table j as ",
      "with m as (select site_id, poc, begin_local, obs_count, mean8 from read_parquet(<<sql_path(mine)>>) ",
      "where hour(begin_local) >= 7), ",
      "e as (select site_id, poc, begin_local, obs_count, mean_all from read_parquet(<<sql_path(epa)>>) ",
      "where parameter_code = '44201'), ",
      "x as (select coalesce(m.site_id, e.site_id) as site_id, coalesce(m.poc, e.poc) as poc, ",
      "coalesce(m.begin_local, e.begin_local) as begin_local, m.obs_count as mine_obs, e.obs_count as epa_obs, ",
      "m.mean8 as mine_value, e.mean_all as epa_value from m full join e ",
      "on m.site_id = e.site_id and m.poc = e.poc and m.begin_local = e.begin_local) ",
      "select '44201' as parameter_code, '8-HR RUN AVG BEGIN HOUR' as sample_duration, *, ",
      "case when mine_value is null then 'missing_in_rebuild' when epa_value is null then 'extra_in_rebuild' ",
      "when abs(mine_value - epa_value) < 1e-9 and mine_obs = epa_obs then 'match' else 'value_differs' end as status, ",
      "case when epa_value is null and mine_obs is not null then 'not_in_epa_file' ",
      "when mine_value is null then 'not_rebuilt' when mine_obs <> epa_obs then 'hour_count' ",
      "when abs(mine_value - epa_value) >= 1e-9 then 'value' else 'none' end as reason from x"
    )))
    write_recon(con, "eighthour", year, out)
  })
}

reconcile_daily <- function(cfg, mine_files, epa_files, year) {
  out <- recon_paths(cfg, "daily", year)
  with_duck(cfg, function(con) {
    duck_exec(con, fill(paste0(
      "create temp table m as select * from read_parquet(<<sql_paths(mine_files)>>)"
    )))
    duck_exec(con, fill(paste0(
      "create temp table e as select site_id, parameter_code, poc, sample_duration, date_local, obs_count, mean, ",
      "max_value, max_hour, aqi, last_change from read_parquet(<<sql_paths(epa_files)>>) ",
      "where event_type in ('None', 'Included') ",
      "and (parameter_code, sample_duration) in (select distinct parameter_code, sample_duration from m)"
    )))
    t640 <- paste(sql_lit(corrected_t640_methods()), collapse = ", ")
    duck_exec(con, fill(paste0(
      "create temp table j as ",
      "with x as (select coalesce(m.site_id, e.site_id) as site_id, coalesce(m.parameter_code, e.parameter_code) as parameter_code, ",
      "coalesce(m.poc, e.poc) as poc, coalesce(m.sample_duration, e.sample_duration) as sample_duration, ",
      "coalesce(m.date_local, e.date_local) as date_local, ",
      "m.obs_count as mine_obs, e.obs_count as epa_obs, m.mean as mine_mean, e.mean as epa_mean, ",
      "m.max_value as mine_max, e.max_value as epa_max, m.max_hour as mine_max_hour, e.max_hour as epa_max_hour, ",
      "m.aqi as mine_aqi, e.aqi as epa_aqi, m.hourly_last_change, e.last_change as epa_last_change, m.method_code, ",
      "m.site_id is not null as in_mine, e.site_id is not null as in_epa ",
      "from m full join e on m.site_id = e.site_id and m.parameter_code = e.parameter_code and m.poc = e.poc ",
      "and m.sample_duration = e.sample_duration and m.date_local = e.date_local), ",
      "y as (select *, ",
      "concat_ws(',', ",
      "case when sample_duration not in ('24-HR BLK AVG', '24 HOUR') and mine_obs is distinct from epa_obs then 'obs' end, ",
      "case when abs(mine_mean - epa_mean) > 1.5e-6 then 'mean' end, ",
      "case when abs(mine_max - epa_max) > 1.5e-6 then 'max' end, ",
      "case when sample_duration not in ('24-HR BLK AVG', '24 HOUR') and mine_max_hour is distinct from epa_max_hour then 'max_hour' end, ",
      "case when epa_aqi is not null and mine_aqi is distinct from epa_aqi then 'aqi' end) as fields from x) ",
      "select * exclude (in_mine, in_epa, fields), ",
      "case when not in_mine then 'missing_in_rebuild' when not in_epa then 'extra_in_rebuild' ",
      "when fields = '' then 'match' else 'value_differs' end as status, ",
      "case when not in_mine then 'not_rebuilt' when fields = '' and in_epa then 'none' ",
      "when hourly_last_change > epa_last_change then 'hourly_revised_after_daily' ",
      "when method_code in (<<t640>>) then 'corrected_t640_method' when not in_epa then 'not_in_epa_file' ",
      "when fields = 'max_hour' then 'max_hour_tie' else fields end as reason from y"
    )))
    write_recon(con, "daily", year, out)
  })
}

reconcile_area <- function(cfg, level, mine, epa, aqi_files, year) {
  out <- recon_paths(cfg, level, year)
  keys <- if (level == "county") c("state_code", "county_code") else "cbsa_code"
  on <- paste(sprintf("m.%s = e.%s", c(keys, "date_local"), c(keys, "date_local")), collapse = " and ")
  pick <- paste(sprintf("coalesce(m.%s, e.%s) as %s", keys, keys, keys), collapse = ", ")
  with_duck(cfg, function(con) {
    duck_exec(con, paste0("create temp table inputs as ", area_inputs_sql(aqi_files)))
    duck_exec(con, fill(paste0(
      "create temp table x as select <<pick>>, coalesce(m.date_local, e.date_local) as date_local, ",
      "m.aqi as mine_aqi, e.aqi as epa_aqi, m.category as mine_category, e.category as epa_category, ",
      "m.defining_parameter as mine_parameter, e.defining_parameter as epa_parameter, ",
      "m.defining_site as mine_site, e.defining_site as epa_site, m.n_sites as mine_sites, e.n_sites as epa_sites ",
      "from read_parquet(<<sql_path(mine)>>) m full join read_parquet(<<sql_path(epa)>>) e on <<on>>"
    )))
    duck_exec(con, paste0(
      "create temp table j as select null::varchar as parameter_code, null::varchar as sample_duration, x.*, ",
      "case when mine_aqi is null then 'missing_in_rebuild' when epa_aqi is null then 'extra_in_rebuild' ",
      "when mine_aqi <> epa_aqi then 'aqi_differs' ",
      "when mine_parameter = epa_parameter and mine_site = epa_site and mine_sites = epa_sites then 'match' ",
      "else 'aqi_match' end as status, ",
      "case when mine_aqi is null then 'not_rebuilt' when epa_aqi is null then 'not_in_epa_file' ",
      "when mine_aqi = epa_aqi and mine_parameter = epa_parameter and mine_site = epa_site and mine_sites = epa_sites then 'none' ",
      "when mine_aqi <> epa_aqi and t.site_aqi is null then 'epa_defining_site_not_rebuilt' ",
      "when mine_aqi < epa_aqi then 'epa_defining_site_lower_in_rebuild' ",
      "when mine_aqi > epa_aqi and t.site_aqi > epa_aqi then 'epa_defining_site_higher_in_rebuild' ",
      "when mine_aqi > epa_aqi then 'rebuild_found_higher_site' ",
      "when mine_sites <> epa_sites then 'site_count' else 'defining_differs' end as reason ",
      "from x left join (select x.date_local as t_date, x.epa_site as t_site, max(i.aqi) as site_aqi ",
      "from x join inputs i on i.date_local = x.date_local and i.site_id = x.epa_site group by all) t ",
      "on t.t_date = x.date_local and t.t_site = x.epa_site"
    ))
    write_recon(con, level, year, out)
  })
}
