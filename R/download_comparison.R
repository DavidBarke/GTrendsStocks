download_comparison <- function(keywords) {
  switch_cookies()

  gtrendsR::gtrends(
    keywords = keywords
  )
}
