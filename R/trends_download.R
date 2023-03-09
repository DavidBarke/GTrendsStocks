#' @export
download_d30_trends <- function(
    ticker = dow_jones_30_ticker(),
    keyword_specs = c(
      ticker = "{TICKER}",
      stock = "{TICKER} stock",
      stock_price = "{TICKER} stock price",
      company_name = "{COMPANY_NAME}",
      company_name_short = "{COMPANY_NAME_SHORT}"
    ),
    int = lubridate::interval("20221201", "20221231"),
    chunk_duration = lubridate::days(120),
    geo = "US",
    interest_only = TRUE
) {
  stopifnot(lubridate::is.interval(int))

  lubridate::int_end(int) <- lubridate::int_end(int) + lubridate::days(1)
  end_date <- lubridate::int_end(int)
  ints <- int_split(int, chunk_duration = chunk_duration)

  dj30 <- dow_jones_30()

  pb <- progress::progress_bar$new(
    total = length(ints) * length(ticker) * length(keyword_specs)
  )

  purrr::map2_dfr(ints, seq_along(ints), function(int, i) {
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
              lubridate::ymd(lubridate::int_start(int)),
              min(
                as.Date(lubridate::ymd(lubridate::int_end(int)) + lubridate::days(60)),
                as.Date(end_date)
              )
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
              keyword_spec = character(),
              chunk = i
            )
          } else {
            trends <- trends %>% dplyr::mutate(
              hits = as.numeric(hits),
              ticker = ticker,
              keyword_spec = keyword_spec,
              chunk = i
            ) %>%
              dplyr::as_tibble() %>%
              dplyr::select(
                date, score = hits, keyword, ticker, keyword_spec, chunk
              )
          }

          pb$tick()
          Sys.sleep(1)
          trends
        })
    })
  })
}

#' @import gtrendsR
#' @import tibble
#' @import readr
#' @import glue
#' @import progress
#'
#' @export
download_trends <- function(
    keywords,
    int = lubridate::interval("20221201", "20221231"),
    chunk_duration = lubridate::days(160),
    overlap = lubridate::days(80),
    geo = "US",
    interest_only = TRUE
) {
  stopifnot(lubridate::is.interval(int))

  lubridate::int_end(int) <- lubridate::int_end(int) + lubridate::days(1)
  end_date <- lubridate::int_end(int)
  ints <- int_split(int, chunk_duration = chunk_duration)

  purrr::map_dfr(keywords, function(keyword) {
    purrr::map2_dfr(ints, seq_along(ints), function(int, i) {
      trends <- gtrendsR::gtrends(
        keyword = keyword,
        geo = geo,
        time = paste(
          lubridate::ymd(lubridate::int_start(int)),
          min(
            as.Date(lubridate::ymd(lubridate::int_end(int)) + overlap),
            as.Date(end_date)
          )
        ),
        onlyInterest = interest_only
      ) %>%
        `[[`("interest_over_time")

      if (is.null(trends)) {
        trends <- tibble::tibble(
          date = lubridate::Date(),
          score = numeric(),
          ticker = character(),
          chunk = i
        )
      } else {
        trends <- trends %>% dplyr::mutate(
          hits = as_hits(hits),
          keyword = keyword,
          chunk = i
        ) %>%
          dplyr::as_tibble() %>%
          dplyr::select(
            date, score = hits, keyword, chunk
          )
      }

      Sys.sleep(1)
      trends
    })
  })
}

#' @export
scale_trends <- function(trends) {
  chunks <- sort(unique(trends$chunk))

  for (chunk in chunks[-length(chunks)]) {
    chunk_1 <- chunk
    chunk_2 <- chunk + 1

    overlapping_dates <- intersect(
      trends[trends$chunk == chunk_1, ]$date,
      trends[trends$chunk == chunk_2, ]$date
    )

    scores_1 <- trends %>%
      dplyr::filter(chunk == chunk_1, date %in% overlapping_dates) %>%
      dplyr::pull(score)

    scores_2 <- trends %>%
      dplyr::filter(chunk == chunk_2, date %in% overlapping_dates) %>%
      dplyr::pull(score)

    scaling_factor <- mean(scores_1 / scores_2, na.rm = TRUE)

    if (scaling_factor > 1) {
      trends[trends$chunk <= chunk_1, ]$score <-
        trends[trends$chunk <= chunk_1, ]$score / scaling_factor
    } else {
      trends[trends$chunk == chunk_2, ]$score <-
        trends[trends$chunk == chunk_2, ]$score * scaling_factor
    }
  }

  trends
}

#' @export
save_trends <- function(trends, filename, basedir = "./trends") {
  first_letter <- substr(filename, 1, 1)

  readr::write_rds(
    trends,
    paste0(file.path(basedir, first_letter, filename), ".rds")
  )
}


#' Split a time interval in chunks of same duration
#'
#' @param int An interval object
#' @param chunk_duration Duration of interval
int_split <- function(int, chunk_duration = lubridate::days(180)) {
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


