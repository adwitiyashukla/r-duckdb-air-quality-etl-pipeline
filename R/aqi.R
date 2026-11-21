read_breakpoints <- function(path) {
  bp <- utils::read.csv(path, colClasses = c("character", "numeric", "numeric", "integer", "integer"))
  checkmate::assert_names(names(bp), identical.to = c("table", "conc_lo", "conc_hi", "aqi_lo", "aqi_hi"))
  missing <- setdiff(unique(pollutants()$breakpoints), bp$table)
  if (length(missing) > 0) {
    cli::cli_abort("Breakpoint tables missing from {.path {path}}: {missing}")
  }
  for (tbl in unique(bp$table)) {
    rows <- bp[bp$table == tbl, ]
    bad <- is.unsorted(rows$conc_lo, strictly = TRUE) || any(rows$conc_hi < rows$conc_lo) ||
      is.unsorted(rows$aqi_lo, strictly = TRUE)
    if (bad) {
      cli::cli_abort("Breakpoint table {tbl} must be strictly increasing")
    }
  }
  tibble::as_tibble(bp)
}

truncate_to <- function(x, digits) {
  scale <- 10^digits
  sign(x) * floor(abs(x) * scale + 1e-7) / scale
}

round_half_up <- function(x, digits = 0) {
  scale <- 10^digits
  sign(x) * floor(abs(x) * scale + 0.5 + 1e-7) / scale
}

aqi_category <- function(aqi) {
  labels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
  as.character(cut(aqi, breaks = c(-Inf, 50, 100, 150, 200, 300, Inf), labels = labels))
}

breakpoint_rows <- function(table, breakpoints) {
  rows <- breakpoints[breakpoints$table == table, ]
  if (nrow(rows) == 0) {
    cli::cli_abort("Unknown breakpoint table {.val {table}}")
  }
  rows
}

aqi_value <- function(concentration, table, breakpoints, digits) {
  rows <- breakpoint_rows(table, breakpoints)
  c_t <- pmax(truncate_to(concentration, digits), 0)
  k <- pmax(findInterval(c_t, rows$conc_lo), 1L)
  slope <- (rows$aqi_hi[k] - rows$aqi_lo[k]) / (rows$conc_hi[k] - rows$conc_lo[k])
  out <- floor(slope * (c_t - rows$conc_lo[k]) + rows$aqi_lo[k] + 0.5 + 1e-7)
  if (table == "so2_1h") {
    out[c_t > rows$conc_hi[nrow(rows)]] <- 200
  }
  out[is.na(concentration)] <- NA
  as.integer(out)
}

aqi_sql <- function(conc, table, breakpoints, digits) {
  rows <- breakpoint_rows(table, breakpoints)
  ct <- sprintf("greatest(trunc_to(%s, %d), 0)", conc, digits)
  num <- function(x) format(x, nsmall = digits, scientific = FALSE, trim = TRUE)
  arms <- vapply(rev(seq_len(nrow(rows))), function(k) {
    r <- rows[k, ]
    slope <- sprintf("%.12f", (r$aqi_hi - r$aqi_lo) / (r$conc_hi - r$conc_lo))
    sprintf("when %s >= %s then floor(%s * (%s - %s) + %d + 0.5 + 1e-7)", ct, num(r$conc_lo), slope, ct, num(r$conc_lo), r$aqi_lo)
  }, character(1))
  if (table == "so2_1h") {
    arms <- c(sprintf("when %s > %s then 200", ct, num(rows$conc_hi[nrow(rows)])), arms)
  }
  paste0("(case when ", conc, " is null then null ", paste(arms, collapse = " "), " end)::integer")
}

category_sql <- function(aqi) {
  paste0(
    "(case when ", aqi, " is null then null when ", aqi, " <= 50 then 'Good' when ", aqi, " <= 100 then 'Moderate' ",
    "when ", aqi, " <= 150 then 'Unhealthy for Sensitive Groups' when ", aqi, " <= 200 then 'Unhealthy' ",
    "when ", aqi, " <= 300 then 'Very Unhealthy' else 'Hazardous' end)"
  )
}

define_macros <- function(con) {
  duck_exec(con, paste0(
    "create or replace macro trunc_to(x, d) as ",
    "case when x >= 0 then floor(x * pow(10, d) + 1e-7) / pow(10, d) else ceil(x * pow(10, d) - 1e-7) / pow(10, d) end"
  ))
  duck_exec(con, "create or replace macro round_half_up(x, d) as sign(x) * floor(abs(x) * pow(10, d) + 0.5 + 1e-7) / pow(10, d)")
  invisible(con)
}
