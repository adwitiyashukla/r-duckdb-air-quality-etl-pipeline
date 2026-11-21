read_config <- function(file = "config.yml", profile = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  if (!file.exists(file)) {
    cli::cli_abort("Config file {.path {file}} not found")
  }
  cfg <- config::get(file = file, config = profile, use_parent = FALSE)
  validate_config(cfg)
}

validate_config <- function(cfg) {
  checkmate::assert_list(cfg, .var.name = "config")
  checkmate::assert_integerish(cfg$years, len = 2, lower = 1980, upper = 2100, any.missing = FALSE, .var.name = "years")
  if (cfg$years[[1]] > cfg$years[[2]]) {
    cli::cli_abort("years must be [first, last] with first <= last, got {cfg$years}")
  }
  cfg$years <- as.integer(cfg$years)

  src <- cfg$source
  checkmate::assert_list(src, .var.name = "source")
  checkmate::assert_string(src$base_url, min.chars = 1, .var.name = "source.base_url")
  checkmate::assert_string(src$user_agent, min.chars = 1, .var.name = "source.user_agent")
  checkmate::assert_number(src$timeout_seconds, lower = 1, .var.name = "source.timeout_seconds")
  checkmate::assert_count(src$max_tries, positive = TRUE, .var.name = "source.max_tries")
  checkmate::assert_count(src$parallel_probes, positive = TRUE, .var.name = "source.parallel_probes")
  cfg$source$base_url <- sub("/+$", "", src$base_url)

  checkmate::assert_string(cfg$paths$data, min.chars = 1, .var.name = "paths.data")
  checkmate::assert_string(cfg$paths$breakpoints, min.chars = 1, .var.name = "paths.breakpoints")
  checkmate::assert_flag(cfg$landing$keep, .var.name = "landing.keep")

  checkmate::assert_string(cfg$duckdb$memory_limit, pattern = "^[0-9]+(\\.[0-9]+)?(MB|GB)$", .var.name = "duckdb.memory_limit")
  checkmate::assert_count(cfg$duckdb$threads, positive = TRUE, .var.name = "duckdb.threads")

  checkmate::assert_list(cfg$bounds, names = "unique", .var.name = "bounds")
  checkmate::assert_set_equal(names(cfg$bounds), pollutant_codes(), .var.name = "bounds")
  for (code in names(cfg$bounds)) {
    b <- cfg$bounds[[code]]
    checkmate::assert_numeric(b, len = 2, any.missing = FALSE, .var.name = paste0("bounds.", code))
    if (b[[1]] >= b[[2]]) {
      cli::cli_abort("bounds.{code} must be [low, high] with low < high")
    }
  }

  q <- cfg$quality
  rates <- c(
    "max_reject_rate", "max_out_of_bounds_rate", "eighthour_match_min", "daily_match_min", "county_match_min", "cbsa_match_min"
  )
  for (key in rates) {
    checkmate::assert_number(q[[key]], lower = 0, upper = 1, .var.name = paste0("quality.", key))
  }

  checkmate::assert_int(cfg$serve$api_port, lower = 1, upper = 65535, .var.name = "serve.api_port")
  checkmate::assert_int(cfg$serve$app_port, lower = 1, upper = 65535, .var.name = "serve.app_port")
  cfg
}

data_dir <- function(cfg, ...) {
  file.path(cfg$paths$data, ...)
}
