#' @import dplyr
#'
#' @export
tidy_crsp <- function(crsp) {
  crsp %>%
    dplyr::select(
      ticker = Ticker,
      return = DlyRet,
      volume = DlyVol,
      high = DlyHigh,
      low = DlyLow,
      open = DlyOpen,
      close = DlyClose,
      date = YYYYMMDD
    ) %>%
    dplyr::mutate(
      date = lubridate::ymd(date)
    )
}

#' @export
merge_trends_with_crsp <- function(
    trends, crsp
) {
  trends %>% dplyr::inner_join(
    crsp, by = c("date", "ticker")
  )
}
