paste_with <- function(str, sep = " ", collapse = NULL, recycle0 = FALSE) {
  function(x) {
    paste(x, str, sep = sep, collapse = collapse, recycle0 = recycle0)
  }
}

#' @import readr
read_dow_jones_csv <- function() {
  readr::read_csv(
    file = system.file(
      "extdata", "dow_jones_30.csv",
      package = "GTrendsStocks"
    ),
    col_types = "ccc"
  ) %>%
    suppressMessages()
}

read_russell_3000_csv <- function(year) {
  readr::read_csv(
    file = system.file(
      "extdata/russell_3000", paste0("members_", year, ".csv"),
      package = "GTrendsStocks"
    ),
    col_types = "cc"
  ) %>%
    suppressMessages()
}

read_sp_500_csv <- function() {
  readr::read_csv(
    file = system.file(
      "extdata/sp_500/sp_500.csv",
      package = "GTrendsStocks"
    ),
    col_types = "cc"
  ) %>%
    suppressMessages()
}

google_trends_period <- function(level = c("daily", "weekly", "monthly")) {
  level <- match.arg(level)

  switch (level,
    daily = lubridate::days(180),
    weekly = lubridate::days(400),
    monthly = lubridate::years(4)
  )
}

as_hits <- function(x, less_than_one = -1) {
  x[x == "<1"] <- less_than_one
  as.numeric(x)
}

switch_cookies <- function() {
  # Print current cookies
  if (exists("cookie_handler", envir = gtrendsR:::.pkgenv)) {
    str(curl::handle_cookies(gtrendsR:::.pkgenv$cookie_handler))
  }
  cookie_env <- gtrendsR:::.pkgenv
  rm("cookie_handler", envir = cookie_env)
  # Assume that TOR is running on 9050
  assign("handle_proxyhost", "socks5://localhost:9050", envir = gtrendsR:::.pkgenv)
}

#' @export
average_return <- function(ret) {
  ret <- ret[!is.na(ret)]
  prod(1 + ret)^(1/length(ret)) - 1
}

#' @export
week_start <- function(n_weeks, origin = as.Date("1999-12-27")) {
  origin + lubridate::days(7) * n_weeks
}

#' @export
paste_time <- function(x, options) {
  if (!length(x) || nchar(x) == 0) {
    paste0(options$time_start, "_", options$time_end)
  } else {
    paste0(x, "_", options$time_start, "_", options$time_end)
  }
}

#' @export
paste_time0 <- function(options) {
  paste0(options$time_start, "_", options$time_end)
}

#' @export
mean2 <- function(x) {
  x <- x[!is.na(x)]
  s <- sd(x)
  m <- mean(x)
  x <- x[x <= m + 3 * s & x >= m - 3 * s]
  mean(x)
}
