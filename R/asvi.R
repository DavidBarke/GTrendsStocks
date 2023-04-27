#' @import TTR zoo
#'
#' @export
median_attention <- function(attention, date, n) {
  if (length(attention) < n) return(NA)
  if (sum(!is.na(attention)) < n) return(NA)
  ts <- zoo::zoo(attention, date)
  TTR::runMedian(ts, n)
}

sd_attention <- function(attention, n) {
  if (length(attention) < n) return(NA)
  TTR::runSD(attention, n = n)
}
