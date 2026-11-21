runtime_settings <- function(cfg) {
  list(source = cfg$source, duckdb = cfg$duckdb, landing = cfg$landing, serve = cfg$serve)
}

job <- function(...) {
  utils::modifyList(getOption("aq.runtime", list()), list(...))
}

pick_file <- function(files, layer) {
  hit <- files[grepl(paste0("/", layer, "/"), files, fixed = TRUE)]
  if (length(hit) == 0) {
    cli::cli_abort("No {layer} output among {.path {files}}")
  }
  hit
}

src_literal <- function(catalog, id) {
  row <- catalog[catalog$id == id, ]
  list(id = row$id, kind = row$kind, parameter_code = row$parameter_code, year = row$year, file = row$file, url = row$url)
}

bronze_name <- function(id) {
  paste0("bronze_", id)
}

bronze_sym <- function(kind, code, year) {
  as.name(bronze_name(source_id(kind, code, year)))
}

call_c <- function(symbols) {
  as.call(c(as.name("c"), symbols))
}


source_targets <- function(catalog) {
  unlist(lapply(catalog$id, function(id) {
    remote <- paste0("remote_", id)
    list(
      targets::tar_target_raw(remote, bquote(remote_index[remote_index$id == .(id), ])),
      targets::tar_target_raw(
        bronze_name(id),
        bquote(ingest_source(.(src_literal(catalog, id)), .(as.name(remote)), job(paths = paths))),
        format = "file"
      )
    )
  }), recursive = FALSE)
}

rebuild_targets <- function(years) {
  codes <- pollutant_codes()
  unlist(lapply(years, function(y) {
    lapply(codes, function(code) {
      targets::tar_target_raw(
        sprintf("rebuild_%s_%d", code, y),
        bquote(rebuild_pollutant(
          job(paths = paths), .(code), .(y), .(bronze_sym("hourly", code, y)),
          .(bronze_sym("daily", code, y)), breakpoints
        )),
        format = "file"
      )
    })
  }), recursive = FALSE)
}

year_targets <- function(years) {
  codes <- pollutant_codes()
  unlist(lapply(years, function(y) {
    rebuilds <- lapply(codes, function(code) as.name(sprintf("rebuild_%s_%d", code, y)))
    aqi_files <- bquote(pick_file(.(call_c(rebuilds)), "aqi"))
    daily_files <- bquote(pick_file(.(call_c(rebuilds)), "daily"))
    epa_daily <- call_c(lapply(codes, function(code) bronze_sym("daily", code, y)))
    hourly <- call_c(lapply(codes, function(code) bronze_sym("hourly", code, y)))
    area <- as.name(sprintf("area_%d", y))
    recon <- lapply(c("eighthour", "daily", "county", "cbsa"), function(l) as.name(sprintf("recon_%s_%d", l, y)))
    year_bronze <- call_c(c(
      lapply(codes, function(code) bronze_sym("hourly", code, y)), lapply(codes, function(code) bronze_sym("daily", code, y)),
      list(bronze_sym("eighthour", NA, y), bronze_sym("aqi_county", NA, y), bronze_sym("aqi_cbsa", NA, y))
    ))
    list(
      targets::tar_target_raw(
        sprintf("area_%d", y),
        bquote(build_area_aqi(job(paths = paths), .(aqi_files), .(epa_daily), .(bronze_sym("aqi_cbsa", NA, y)), .(y))),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("recon_eighthour_%d", y),
        bquote(reconcile_eighthour(
          job(paths = paths), pick_file(.(as.name(sprintf("rebuild_44201_%d", y))), "eighthour"),
          .(bronze_sym("eighthour", NA, y)), .(y)
        )),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("recon_daily_%d", y),
        bquote(reconcile_daily(job(paths = paths), .(daily_files), .(epa_daily), .(y))),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("recon_county_%d", y),
        bquote(reconcile_area(
          job(paths = paths), "county", pick_file(.(area), "county_aqi"), .(bronze_sym("aqi_county", NA, y)),
          .(aqi_files), .(y)
        )),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("recon_cbsa_%d", y),
        bquote(reconcile_area(
          job(paths = paths), "cbsa", pick_file(.(area), "cbsa_aqi"), .(bronze_sym("aqi_cbsa", NA, y)),
          .(aqi_files), .(y)
        )),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("quality_hourly_%d", y),
        bquote(contract_hourly(job(paths = paths, bounds = bounds, quality = quality), .(hourly), .(y))),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("quality_rejects_%d", y),
        bquote(contract_rejects(job(paths = paths, quality = quality), .(year_bronze), .(y))),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("quality_area_%d", y),
        bquote(contract_area(
          job(paths = paths), list(county = pick_file(.(area), "county_aqi"), cbsa = pick_file(.(area), "cbsa_aqi")), .(y)
        )),
        format = "file"
      ),
      targets::tar_target_raw(
        sprintf("quality_recon_%d", y),
        bquote(contract_reconciliation(
          job(paths = paths, quality = quality),
          grep("_summary", .(call_c(recon)), value = TRUE, fixed = TRUE), .(y)
        )),
        format = "file"
      )
    )
  }), recursive = FALSE)
}

warehouse_target <- function(catalog, years) {
  codes <- pollutant_codes()
  per_year <- function(fmt) call_c(lapply(years, function(y) as.name(sprintf(fmt, y))))
  rebuilds <- call_c(unlist(lapply(years, function(y) {
    lapply(codes, function(code) as.name(sprintf("rebuild_%s_%d", code, y)))
  })))
  recon <- as.call(c(as.name("list"), stats::setNames(lapply(c("eighthour", "daily", "county", "cbsa"), function(l) {
    per_year(paste0("recon_", l, "_%d"))
  }), c("eighthour", "daily", "county", "cbsa"))))
  quality <- call_c(lapply(c("hourly", "rejects", "area", "recon"), function(k) per_year(paste0("quality_", k, "_%d"))))
  bronze <- call_c(lapply(catalog$id, function(id) as.name(bronze_name(id))))
  areas <- per_year("area_%d")
  targets::tar_target_raw(
    "warehouse",
    bquote(build_warehouse(
      job(paths = paths, years = years), catalog, .(bronze), pick_file(.(areas), "county_aqi"),
      pick_file(.(areas), "cbsa_aqi"), pick_file(.(rebuilds), "aqi"), .(recon), .(quality), breakpoints
    )),
    format = "file"
  )
}

pipeline_targets <- function(cfg) {
  catalog <- source_catalog(cfg)
  years <- seq(cfg$years[[1]], cfg$years[[2]])
  c(
    list(
      targets::tar_target_raw("breakpoints_file", quote(paths$breakpoints), format = "file"),
      targets::tar_target_raw("breakpoints", quote(read_breakpoints(breakpoints_file))),
      targets::tar_target_raw("catalog", bquote(source_catalog(list(years = years, source = list(base_url = .(cfg$source$base_url)))))),
      targets::tar_target_raw("remote_index", quote(probe_sources(catalog, job())), cue = targets::tar_cue(mode = "always"))
    ),
    source_targets(catalog),
    rebuild_targets(years),
    year_targets(years),
    list(
      warehouse_target(catalog, years),
      targets::tar_target_raw("report", quote(build_report(job(paths = paths, years = years), warehouse)), format = "file")
    )
  )
}
