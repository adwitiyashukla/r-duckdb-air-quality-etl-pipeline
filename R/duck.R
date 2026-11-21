duck_connect <- function(cfg) {
  tmp <- data_dir(cfg, "tmp")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  DBI::dbExecute(con, paste0("set memory_limit = ", sql_lit(cfg$duckdb$memory_limit)))
  DBI::dbExecute(con, paste0("set threads = ", as.integer(cfg$duckdb$threads)))
  DBI::dbExecute(con, paste0("set temp_directory = ", sql_path(tmp)))
  con
}

with_duck <- function(cfg, fn) {
  con <- duck_connect(cfg)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  fn(con)
}

sql_lit <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

sql_path <- function(path) {
  sql_lit(normalizePath(path, winslash = "/", mustWork = FALSE))
}

sql_paths <- function(paths) {
  paste0("[", paste(vapply(paths, sql_path, character(1)), collapse = ", "), "]")
}

fill <- function(template, ...) {
  glue::glue(template, ..., .open = "<<", .close = ">>", .envir = parent.frame())
}

duck_exec <- function(con, sql) {
  DBI::dbExecute(con, sql)
}

duck_get <- function(con, sql) {
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

write_parquet <- function(con, select_sql, path, kv = list()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(path, ".part")
  unlink(part)
  options <- "format parquet, compression zstd"
  if (length(kv) > 0) {
    pairs <- paste(sprintf("%s: %s", names(kv), vapply(kv, sql_lit, character(1))), collapse = ", ")
    options <- paste0(options, ", kv_metadata {", pairs, "}")
  }
  duck_exec(con, paste0("copy (", select_sql, ") to ", sql_path(part), " (", options, ")"))
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(part, path)) {
    cli::cli_abort("Could not write {.path {path}}")
  }
  path
}

parquet_rows <- function(con, path) {
  duck_get(con, paste0("select count(*)::bigint as n from read_parquet(", sql_path(path), ")"))$n
}

parquet_kv <- function(con, path) {
  kv <- duck_get(con, paste0(
    "select decode(key) as key, decode(value) as value from parquet_kv_metadata(", sql_path(path), ")"
  ))
  stats::setNames(as.list(kv$value), kv$key)
}
