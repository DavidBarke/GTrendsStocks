#' @export
dow_jones_30 <- function() {
  read_dow_jones_csv()
}

#' @export
dow_jones_30_ticker <- function() {
  dow_jones_30()$TICKER
}


