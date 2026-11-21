warehouse_path <- function(cfg) {
  data_dir(cfg, "warehouse.duckdb")
}

source_manifest <- function(con, catalog, bronze_files) {
  rows <- lapply(seq_along(bronze_files), function(i) {
    kv <- parquet_kv(con, bronze_files[[i]])
    tibble::tibble(
      id = catalog$id[[i]], kind = catalog$kind[[i]], parameter_code = catalog$parameter_code[[i]],
      year = catalog$year[[i]], role = catalog$role[[i]], source_file = kv$source_file, source_url = kv$source_url,
      etag = kv$etag, last_modified = kv$last_modified, bytes = as.numeric(kv$bytes), sha256 = kv$sha256,
      rows_read = as.numeric(kv$rows_read), rows_rejected = as.numeric(kv$rows_rejected),
      parquet_bytes = file.info(bronze_files[[i]])$size
    )
  })
  do.call(rbind, rows)
}

build_warehouse <- function(cfg, catalog, bronze_files, county_files, cbsa_files, aqi_files, recon_files, quality_files, breakpoints) {
  path <- warehouse_path(cfg)
  part <- paste0(path, ".part")
  unlink(c(part, paste0(part, ".wal")))
  bronze <- function(kind) unname(bronze_files[catalog$kind == kind])
  mismatch <- function(level) recon_files[[level]][grepl("_mismatch", recon_files[[level]])]
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = part)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0("set memory_limit = ", sql_lit(cfg$duckdb$memory_limit)))
  DBI::dbExecute(con, paste0("set threads = ", as.integer(cfg$duckdb$threads)))
  define_macros(con)
  manifest <- source_manifest(con, catalog, bronze_files)
  duckdb::duckdb_register(con, "manifest_df", manifest)
  duck_exec(con, "create table sources as select * from manifest_df")
  duck_exec(con, paste0("create table sites as select * from read_parquet(", sql_path(bronze("sites")), ")"))
  duck_exec(con, paste0("create table monitors as select * from read_parquet(", sql_path(bronze("monitors")), ")"))
  duck_exec(con, fill(paste0(
    "create table county_aqi as select coalesce(m.state_code, e.state_code) as state_code, ",
    "coalesce(m.county_code, e.county_code) as county_code, coalesce(m.date_local, e.date_local) as date_local, ",
    "e.state_name, e.county_name, m.aqi, m.category, m.defining_parameter, m.defining_site, m.n_sites, ",
    "e.aqi as epa_aqi, e.category as epa_category, e.defining_parameter as epa_defining_parameter, ",
    "e.defining_site as epa_defining_site, e.n_sites as epa_n_sites, ",
    "coalesce(r.status, 'match') as status, coalesce(r.reason, 'none') as reason ",
    "from read_parquet(<<sql_paths(county_files)>>) m ",
    "full join read_parquet(<<sql_paths(bronze('aqi_county'))>>) e ",
    "on m.state_code = e.state_code and m.county_code = e.county_code and m.date_local = e.date_local ",
    "left join (select state_code, county_code, date_local, status, reason from read_parquet(<<sql_paths(mismatch('county'))>>)) r ",
    "on r.state_code = coalesce(m.state_code, e.state_code) and r.county_code = coalesce(m.county_code, e.county_code) ",
    "and r.date_local = coalesce(m.date_local, e.date_local)"
  )))
  duck_exec(con, paste0(
    "update county_aqi set state_name = s.state_name, county_name = s.county_name from ",
    "(select state_code, county_code, any_value(state_name) as state_name, any_value(county_name) as county_name ",
    "from sites group by all) s where county_aqi.state_name is null and s.state_code = county_aqi.state_code ",
    "and s.county_code = county_aqi.county_code"
  ))
  duck_exec(con, fill(paste0(
    "create table cbsa_aqi as select coalesce(m.cbsa_code, e.cbsa_code) as cbsa_code, ",
    "coalesce(m.date_local, e.date_local) as date_local, e.cbsa_name, m.aqi, m.category, m.defining_parameter, ",
    "m.defining_site, m.n_sites, e.aqi as epa_aqi, e.category as epa_category, ",
    "e.defining_parameter as epa_defining_parameter, e.defining_site as epa_defining_site, e.n_sites as epa_n_sites, ",
    "coalesce(r.status, 'match') as status, coalesce(r.reason, 'none') as reason ",
    "from read_parquet(<<sql_paths(cbsa_files)>>) m full join read_parquet(<<sql_paths(bronze('aqi_cbsa'))>>) e ",
    "on m.cbsa_code = e.cbsa_code and m.date_local = e.date_local ",
    "left join (select cbsa_code, date_local, status, reason from read_parquet(<<sql_paths(mismatch('cbsa'))>>)) r ",
    "on r.cbsa_code = coalesce(m.cbsa_code, e.cbsa_code) and r.date_local = coalesce(m.date_local, e.date_local)"
  )))
  duck_exec(con, paste0(
    "update cbsa_aqi set cbsa_name = n.cbsa_name from (select cbsa_code, any_value(cbsa_name) as cbsa_name ",
    "from cbsa_aqi where cbsa_name is not null group by all) n where cbsa_aqi.cbsa_name is null and n.cbsa_code = cbsa_aqi.cbsa_code"
  ))
  summaries <- unlist(lapply(recon_files, function(x) x[grepl("_summary", x)]), use.names = FALSE)
  duck_exec(con, paste0("create table recon_summary as select * from read_parquet(", sql_paths(summaries), ", union_by_name = true)"))
  duck_exec(con, paste0(
    "create table recon_mismatch as select level, * exclude (level) from (",
    paste(vapply(names(recon_files), function(level) {
      paste0("select ", sql_lit(level), " as level, * from read_parquet(", sql_paths(mismatch(level)), ", union_by_name = true)")
    }, character(1)), collapse = " union all by name "),
    ")"
  ))
  duck_exec(con, paste0("create table quality as select * from read_parquet(", sql_paths(quality_files), ", union_by_name = true)"))
  duck_exec(con, paste0("create table monitor_aqi as select * from read_parquet(", sql_paths(aqi_files), ", union_by_name = true)"))
  duck_exec(con, paste0(
    "create table county_year as select state_code, county_code, any_value(state_name) as state_name, ",
    "any_value(county_name) as county_name, year(date_local)::integer as year, count(aqi)::integer as days, ",
    "count(*) filter (where category = 'Good')::integer as good, ",
    "count(*) filter (where category = 'Moderate')::integer as moderate, ",
    "count(*) filter (where category = 'Unhealthy for Sensitive Groups')::integer as usg, ",
    "count(*) filter (where category = 'Unhealthy')::integer as unhealthy, ",
    "count(*) filter (where category = 'Very Unhealthy')::integer as very_unhealthy, ",
    "count(*) filter (where category = 'Hazardous')::integer as hazardous, ",
    "max(aqi) as max_aqi, round(avg(aqi), 1) as mean_aqi, ",
    "first(date_local order by aqi desc, date_local) as worst_day from county_aqi where aqi is not null ",
    "group by state_code, county_code, year(date_local)"
  ))
  old_aqi <- aqi_sql("concentration", "pm25_24h_2012", breakpoints, 1L)
  duck_exec(con, paste0(
    "create table pm25_breakpoint_shift as with m as (select substr(site_id, 1, 2) as state_code, ",
    "substr(site_id, 4, 3) as county_code, date_local, case when aqi_parameter = 'PM2.5' then ", old_aqi,
    " else aqi end as aqi_old from monitor_aqi), o as (select state_code, county_code, date_local, max(aqi_old) as aqi_old ",
    "from m group by all) select year(c.date_local)::integer as year, ", category_sql("o.aqi_old"),
    " as category_2012_breakpoints, c.category as category_2024_breakpoints, count(*)::integer as county_days ",
    "from county_aqi c join o using (state_code, county_code, date_local) where c.aqi is not null group by all order by all"
  ))
  info <- tibble::tibble(
    key = c("years", "built_at", "r_version", "duckdb_version", "source"),
    value = c(
      paste(cfg$years, collapse = "-"), format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      paste(R.version$major, R.version$minor, sep = "."), as.character(utils::packageVersion("duckdb")),
      cfg$source$base_url
    )
  )
  duckdb::duckdb_register(con, "info_df", info)
  duck_exec(con, "create table build_info as select * from info_df")
  duck_exec(con, "checkpoint")
  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit()
  unlink(c(path, paste0(path, ".wal")))
  if (!file.rename(part, path)) {
    cli::cli_abort("Could not move {.path {part}} to {.path {path}}")
  }
  path
}
