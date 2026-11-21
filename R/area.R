gold_path <- function(cfg, layer, year) {
  data_dir(cfg, "gold", layer, paste0(year, ".parquet"))
}

area_inputs_sql <- function(aqi_files) {
  fill(paste0(
    "select site_id, parameter_code, poc, date_local, aqi_parameter, sample_duration, concentration, aqi ",
    "from read_parquet(<<sql_paths(aqi_files)>>) where aqi is not null"
  ))
}

area_rollup_sql <- function(inputs, keys) {
  key_list <- paste(keys, collapse = ", ")
  fill(paste0(
    "with m as (<<inputs>>), ",
    "top as (select <<key_list>>, date_local, max(aqi) as aqi, count(distinct site_id)::integer as n_sites ",
    "from m group by all), ",
    "pick as (select <<key_list>>, date_local, min(aqi_parameter) as defining_parameter, max(site_id) as defining_site ",
    "from m join top using (<<key_list>>, date_local, aqi) group by all) ",
    "select <<key_list>>, date_local, aqi, <<category_sql('aqi')>> as category, defining_parameter, defining_site, n_sites ",
    "from top join pick using (<<key_list>>, date_local)"
  ))
}

build_area_aqi <- function(cfg, aqi_files, daily_files, cbsa_file, year) {
  out <- c(county = gold_path(cfg, "county_aqi", year), cbsa = gold_path(cfg, "cbsa_aqi", year))
  with_duck(cfg, function(con) {
    duck_exec(con, paste0("create temp table m as ", area_inputs_sql(aqi_files)))
    county_inputs <- "select substr(site_id, 1, 2) as state_code, substr(site_id, 4, 3) as county_code, * from m"
    write_parquet(
      con,
      paste0(area_rollup_sql(county_inputs, c("state_code", "county_code")), " order by state_code, county_code, date_local"),
      out[["county"]]
    )
    duck_exec(con, fill(paste0(
      "create temp table site_cbsa as select site_id, any_value(cbsa_name) as cbsa_name ",
      "from read_parquet(<<sql_paths(daily_files)>>) where cbsa_name is not null and cbsa_name <> '' group by all"
    )))
    duck_exec(con, fill(paste0(
      "create temp table cbsa_codes as select cbsa_name, any_value(cbsa_code) as cbsa_code ",
      "from read_parquet(<<sql_path(cbsa_file)>>) group by all"
    )))
    cbsa_inputs <- paste0(
      "select c.cbsa_code, m.* from m join site_cbsa s using (site_id) join cbsa_codes c using (cbsa_name)"
    )
    write_parquet(
      con,
      paste0(area_rollup_sql(cbsa_inputs, "cbsa_code"), " order by cbsa_code, date_local"),
      out[["cbsa"]]
    )
  })
  out
}
