source_header <- function(kind) {
  switch(kind,
    hourly = c(
      "State Code", "County Code", "Site Num", "Parameter Code", "POC", "Latitude", "Longitude", "Datum",
      "Parameter Name", "Date Local", "Time Local", "Date GMT", "Time GMT", "Sample Measurement",
      "Units of Measure", "MDL", "Uncertainty", "Qualifier", "Method Type", "Method Code", "Method Name",
      "State Name", "County Name", "Date of Last Change"
    ),
    daily = c(
      "State Code", "County Code", "Site Num", "Parameter Code", "POC", "Latitude", "Longitude", "Datum",
      "Parameter Name", "Sample Duration", "Pollutant Standard", "Date Local", "Units of Measure", "Event Type",
      "Observation Count", "Observation Percent", "Arithmetic Mean", "1st Max Value", "1st Max Hour", "AQI",
      "Method Code", "Method Name", "Local Site Name", "Address", "State Name", "County Name", "City Name",
      "CBSA Name", "Date of Last Change"
    ),
    eighthour = c(
      "State Code", "County Code", "Site Num", "Parameter Code", "POC", "Latitude", "Longitude", "Datum",
      "Parameter Name", "Date Local", "Time Local", "Date GMT", "Time GMT", "Sample Duration", "Pollutant Standard",
      "Units of Measure", "Observation Count", "Observations with Events", "Null Observations",
      "Mean Including All Data", "Mean Excluding All Flagged Data", "Mean Excluding Concurred Flags",
      "Date of Last Change"
    ),
    aqi_county = c(
      "State Name", "county Name", "State Code", "County Code", "Date", "AQI", "Category", "Defining Parameter",
      "Defining Site", "Number of Sites Reporting"
    ),
    aqi_cbsa = c(
      "CBSA", "CBSA Code", "Date", "AQI", "Category", "Defining Parameter", "Defining Site",
      "Number of Sites Reporting"
    ),
    sites = c(
      "State Code", "County Code", "Site Number", "Latitude", "Longitude", "Datum", "Elevation", "Land Use",
      "Location Setting", "Site Established Date", "Site Closed Date", "Met Site State Code",
      "Met Site County Code", "Met Site Site Number", "Met Site Type", "Met Site Distance", "Met Site Direction",
      "GMT Offset", "Owning Agency", "Local Site Name", "Address", "Zip Code", "State Name", "County Name",
      "City Name", "CBSA Name", "Tribe Name", "Extraction Date"
    ),
    monitors = c(
      "State Code", "County Code", "Site Number", "Parameter Code", "Parameter Name", "POC", "Latitude",
      "Longitude", "Datum", "First Year of Data", "Last Sample Date", "Monitor Type", "Networks",
      "Reporting Agency", "PQAO", "Collecting Agency", "Exclusions", "Monitoring Objective", "Last Method Code",
      "Last Method", "Measurement Scale", "Measurement Scale Definition", "NAAQS Primary Monitor",
      "QA Primary Monitor", "Local Site Name", "Address", "State Name", "County Name", "City Name", "CBSA Name",
      "Tribe Name", "Extraction Date"
    ),
    cli::cli_abort("Unknown source kind {.val {kind}}")
  )
}

bronze_select <- function(kind) {
  site <- "\"State Code\" || '-' || \"County Code\" || '-' || \"Site Num\""
  switch(kind,
    hourly = paste0(
      "select ", site, " as site_id, \"Parameter Code\" as parameter_code, try_cast(POC as integer) as poc, ",
      "try_cast(\"Date Local\" || ' ' || \"Time Local\" as timestamp) as time_local, ",
      "try_cast(\"Date GMT\" || ' ' || \"Time GMT\" as timestamp) as time_gmt, ",
      "try_cast(\"Sample Measurement\" as double) as value, \"Units of Measure\" as units, ",
      "try_cast(MDL as double) as mdl, try_cast(Uncertainty as double) as uncertainty, Qualifier as qualifier, ",
      "\"Method Type\" as method_type, \"Method Code\" as method_code, ",
      "try_cast(\"Date of Last Change\" as date) as last_change"
    ),
    daily = paste0(
      "select ", site, " as site_id, \"Parameter Code\" as parameter_code, try_cast(POC as integer) as poc, ",
      "\"Sample Duration\" as sample_duration, \"Pollutant Standard\" as pollutant_standard, ",
      "try_cast(\"Date Local\" as date) as date_local, \"Units of Measure\" as units, \"Event Type\" as event_type, ",
      "try_cast(\"Observation Count\" as integer) as obs_count, try_cast(\"Observation Percent\" as double) as obs_pct, ",
      "try_cast(\"Arithmetic Mean\" as double) as mean, try_cast(\"1st Max Value\" as double) as max_value, ",
      "try_cast(\"1st Max Hour\" as integer) as max_hour, try_cast(AQI as integer) as aqi, ",
      "\"Method Code\" as method_code, \"CBSA Name\" as cbsa_name, ",
      "try_cast(\"Date of Last Change\" as date) as last_change"
    ),
    eighthour = paste0(
      "select ", site, " as site_id, \"Parameter Code\" as parameter_code, try_cast(POC as integer) as poc, ",
      "try_cast(\"Date Local\" || ' ' || \"Time Local\" as timestamp) as begin_local, ",
      "try_cast(\"Observation Count\" as integer) as obs_count, ",
      "try_cast(\"Observations with Events\" as integer) as obs_with_events, ",
      "try_cast(\"Null Observations\" as integer) as null_obs, ",
      "try_cast(\"Mean Including All Data\" as double) as mean_all, ",
      "try_cast(\"Mean Excluding All Flagged Data\" as double) as mean_excluding_flagged, ",
      "try_cast(\"Mean Excluding Concurred Flags\" as double) as mean_excluding_concurred, ",
      "try_cast(\"Date of Last Change\" as date) as last_change"
    ),
    aqi_county = paste0(
      "select \"State Code\" as state_code, \"County Code\" as county_code, \"State Name\" as state_name, ",
      "\"county Name\" as county_name, try_cast(\"Date\" as date) as date_local, try_cast(AQI as integer) as aqi, ",
      "Category as category, \"Defining Parameter\" as defining_parameter, \"Defining Site\" as defining_site, ",
      "try_cast(\"Number of Sites Reporting\" as integer) as n_sites"
    ),
    aqi_cbsa = paste0(
      "select \"CBSA Code\" as cbsa_code, CBSA as cbsa_name, try_cast(\"Date\" as date) as date_local, ",
      "try_cast(AQI as integer) as aqi, Category as category, \"Defining Parameter\" as defining_parameter, ",
      "\"Defining Site\" as defining_site, try_cast(\"Number of Sites Reporting\" as integer) as n_sites"
    ),
    sites = paste0(
      "select \"State Code\" || '-' || \"County Code\" || '-' || \"Site Number\" as site_id, ",
      "\"State Code\" as state_code, \"County Code\" as county_code, try_cast(Latitude as double) as latitude, ",
      "try_cast(Longitude as double) as longitude, Datum as datum, try_cast(Elevation as double) as elevation, ",
      "\"Land Use\" as land_use, \"Location Setting\" as location_setting, ",
      "try_cast(\"Site Established Date\" as date) as established, try_cast(\"Site Closed Date\" as date) as closed, ",
      "try_cast(\"GMT Offset\" as integer) as gmt_offset, \"Owning Agency\" as owning_agency, ",
      "\"Local Site Name\" as local_site_name, Address as address, \"Zip Code\" as zip_code, ",
      "\"State Name\" as state_name, \"County Name\" as county_name, \"City Name\" as city_name, ",
      "\"CBSA Name\" as cbsa_name, \"Tribe Name\" as tribe_name"
    ),
    monitors = paste0(
      "select \"State Code\" || '-' || \"County Code\" || '-' || \"Site Number\" as site_id, ",
      "\"Parameter Code\" as parameter_code, \"Parameter Name\" as parameter_name, try_cast(POC as integer) as poc, ",
      "try_cast(\"First Year of Data\" as integer) as first_year, ",
      "try_cast(\"Last Sample Date\" as date) as last_sample_date, \"Monitor Type\" as monitor_type, ",
      "Networks as networks, \"Reporting Agency\" as reporting_agency, ",
      "\"Monitoring Objective\" as monitoring_objective, \"Last Method Code\" as last_method_code, ",
      "\"Last Method\" as last_method, \"NAAQS Primary Monitor\" as naaqs_primary_monitor"
    ),
    cli::cli_abort("Unknown source kind {.val {kind}}")
  )
}

bronze_filter <- function(kind) {
  switch(kind,
    monitors = paste0("\"Parameter Code\" in (", paste(sql_lit(pollutant_codes()), collapse = ", "), ")"),
    "true"
  )
}

bronze_reject_rule <- function(kind) {
  switch(kind,
    hourly = "poc is null or time_local is null or time_gmt is null or value is null",
    daily = "poc is null or date_local is null",
    eighthour = "poc is null or begin_local is null or obs_count is null",
    aqi_county = "date_local is null or aqi is null",
    aqi_cbsa = "date_local is null or aqi is null",
    "false"
  )
}

zip_entry <- function(zip_path) {
  entries <- utils::unzip(zip_path, list = TRUE)
  csv <- entries$Name[grepl("\\.csv$", entries$Name, ignore.case = TRUE)]
  if (length(csv) != 1) {
    cli::cli_abort("{.path {basename(zip_path)}} should hold exactly one csv, found {length(csv)}")
  }
  csv
}

unpack_csv <- function(zip_path, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  entry <- zip_entry(zip_path)
  target <- file.path(out_dir, paste0(sub("\\.zip$", "", basename(zip_path)), ".csv.gz"))
  input <- unz(zip_path, entry, open = "rb")
  on.exit(close(input), add = TRUE)
  output <- gzfile(target, open = "wb", compression = 1)
  on.exit(close(output), add = TRUE)
  repeat {
    chunk <- readBin(input, "raw", n = 16L * 1024L * 1024L)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, output)
  }
  target
}

read_header <- function(csv_gz) {
  con <- gzfile(csv_gz, open = "r")
  on.exit(close(con), add = TRUE)
  line <- readLines(con, n = 1, warn = FALSE)
  gsub("^\"|\"$", "", strsplit(line, "\",\"", fixed = TRUE)[[1]])
}

check_header <- function(csv_gz, kind, file) {
  got <- read_header(csv_gz)
  want <- source_header(kind)
  if (!identical(got, want)) {
    extra <- setdiff(got, want)
    missing <- setdiff(want, got)
    cli::cli_abort(c(
      "{file} does not have the expected {kind} layout",
      "x" = "missing columns: {if (length(missing)) missing else 'none'}",
      "x" = "unexpected columns: {if (length(extra)) extra else 'none'}"
    ))
  }
  invisible(TRUE)
}

bronze_path <- function(cfg, src) {
  if (src$kind %in% c("sites", "monitors")) {
    return(data_dir(cfg, "bronze", paste0(src$kind, ".parquet")))
  }
  folder <- if (is.na(src$parameter_code)) src$kind else file.path(src$kind, src$parameter_code)
  data_dir(cfg, "bronze", folder, paste0(src$year, ".parquet"))
}

reject_path <- function(cfg, src) {
  data_dir(cfg, "bronze", "_rejects", paste0(src$id, ".parquet"))
}

ingest_source <- function(src, remote, cfg) {
  landing <- fetch_source(src, remote, cfg)
  sha <- secretbase::sha256(file = landing)
  work <- data_dir(cfg, "tmp", src$id)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  csv_gz <- unpack_csv(landing, work)
  check_header(csv_gz, src$kind, src$file)
  out <- bronze_path(cfg, src)
  rejects <- reject_path(cfg, src)
  with_duck(cfg, function(con) {
    duck_exec(con, paste0(
      "create temp table typed as ", bronze_select(src$kind),
      " from read_csv(", sql_path(csv_gz), ", header = true, all_varchar = true, quote = '\"', escape = '\"') ",
      "where ", bronze_filter(src$kind)
    ))
    n_total <- duck_get(con, "select count(*)::bigint as n from typed")$n
    n_reject <- duck_get(con, paste0("select count(*)::bigint as n from typed where ", bronze_reject_rule(src$kind)))$n
    if (n_reject > 0) {
      write_parquet(con, paste0("select * from typed where ", bronze_reject_rule(src$kind)), rejects)
    } else if (file.exists(rejects)) {
      unlink(rejects)
    }
    kv <- list(
      source_file = src$file,
      source_url = src$url,
      etag = if (is.na(remote$etag)) "" else remote$etag,
      last_modified = if (is.na(remote$last_modified)) "" else remote$last_modified,
      bytes = format(remote$bytes, scientific = FALSE),
      sha256 = sha,
      rows_read = format(n_total, scientific = FALSE),
      rows_rejected = format(n_reject, scientific = FALSE)
    )
    write_parquet(con, paste0("select * from typed where not (", bronze_reject_rule(src$kind), ")"), out, kv)
  })
  if (!cfg$landing$keep) {
    unlink(landing)
  }
  out
}
