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
