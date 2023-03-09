#' @export
dow_jones_30 <- function() {
  read_dow_jones_csv()
}

#' @export
dow_jones_30_ticker <- function() {
  dow_jones_30()$TICKER
}

#' @export
russell_3000 <- function(year = 2023) {
  read_russell_3000_csv(year)
}

#' @export
sp_500 <- function() {
  read_sp_500_csv()
}
