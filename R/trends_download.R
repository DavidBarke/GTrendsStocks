#' @import gtrendsR
#'
#' @export
download_trends <- function(
    keywords,
    int,
    chunk_duration = lubridate::days(180),
    geo = "US",
    interest_only = TRUE
) {
  stopifnot(lubridate::is.interval(int))

  lubridate::int_end(int) <- lubridate::int_end(int) + lubridate::days(1)
  ints <- int_split(int, chunk_duration = chunk_duration)

  purrr::map_dfr(ints, function(x) {
    gtrendsR::gtrends(
      keyword = keywords,
      geo = geo,
      time = paste(ymd(int_start(x)), ymd(int_end(x)) - lubridate::days(1)),
      onlyInterest = interest_only
    ) %>%
      dplyr::select(
        date, score = hits, keyword
      )
  })
}


#' Split a time interval in chunks of same duration
#'
#' @param int An interval object
#' @param chunk_duration Duration of interval
int_split <- function(int, chunk_duration = lubridate::months(6)) {
  start <- lubridate::int_start(int)
  end <- lubridate::int_end(int)

  current_date <- start
  start_dates <- current_date
  while (current_date + chunk_duration < end) {
    current_date <- current_date + chunk_duration
    start_dates <- c(start_dates, current_date)
  }

  lubridate::int_diff(c(start_dates, end))
}


