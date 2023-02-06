paste_with <- function(str, sep = " ", collapse = NULL, recycle0 = FALSE) {
  function(x) {
    paste(x, str, sep = sep, collapse = collapse, recycle0 = recycle0)
  }
}

#' @import readr
read_dow_jones_csv <- function() {
  readr::read_csv2(
    file = system.file("extdata", "dow_jones_30.csv", package = "GoogleTrends"),
    col_types = "ccc"
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
