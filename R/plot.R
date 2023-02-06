#' @import plotly
#'
#' @export
plot_trends <- function(trends) {
  plotly::plot_ly(
    data = trends,
    type = "scatter",
    mode = "lines+markers",
    x = ~date,
    y = ~score,
    color = ~keyword
  )
}
