#' @import TTR zoo
#'
#' @export
median_attention <- function(attention, date, n) {
  #print(attention)
  #print(date)
  ts <- zoo::zoo(attention, date)
  runMedian(ts, n)
}
