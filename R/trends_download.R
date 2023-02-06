#' @import gtrendsR
#' @import tibble
#' @import readr
#' @import glue
#' @import progress
#'
#' @export
download_trends <- function(
    ticker = dow_jones_30_ticker(),
    keyword_specs = c(
      ticker = "{TICKER}",
      stock = "{TICKER} stock",
      stock_price = "{TICKER} stock price",
      company_name = "{COMPANY_NAME}",
      company_name_short = "{COMPANY_NAME_SHORT}"
    ),
    int = lubridate::interval("20221201", "20221231"),
    chunk_duration = lubridate::days(180),
    geo = "US",
    interest_only = TRUE
) {
  stopifnot(lubridate::is.interval(int))

  lubridate::int_end(int) <- lubridate::int_end(int) + lubridate::days(1)
  ints <- int_split(int, chunk_duration = chunk_duration)

  dj30 <- dow_jones_30()

  pb <- progress::progress_bar$new(
    total = length(ints) * length(ticker) * length(keyword_specs)
  )

  purrr::map_dfr(ints, function(int) {
    purrr::map_dfr(ticker, function(ticker) {
      dj30_filtered <- dj30 %>%
        dplyr::filter(TICKER == ticker)
      keywords <- purrr::map_chr(keyword_specs, function(keyword_spec) {
        glue::glue_data(dj30_filtered, keyword_spec)
      })

      purrr::map2_dfr(
        keywords, names(keyword_specs),
        function(keyword, keyword_spec) {
          trends <- gtrendsR::gtrends(
            keyword = keyword,
            geo = geo,
            time = paste(
              lubridate::ymd(int_start(int)),
              lubridate::ymd(int_end(int)) - lubridate::days(1)
            ),
            onlyInterest = interest_only
          ) %>%
            `[[`("interest_over_time")

          if (is.null(trends)) {
            trends <- tibble::tibble(
              date = lubridate::Date(),
              score = numeric(),
              keyword = character(),
              ticker = character(),
              keyword_spec = character()
            )
          } else {
            trends <- trends %>% dplyr::mutate(
              hits = as.numeric(hits),
              ticker = ticker,
              keyword_spec = keyword_spec
            ) %>%
              dplyr::as_tibble() %>%
              dplyr::select(
                date, score = hits, keyword, ticker, keyword_spec
              )
          }

          pb$tick()
          Sys.sleep(1)
          trends
        })
    })
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


