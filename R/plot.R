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

#' @export
plot_search_volume <- function(trends, x = "date", y = "score") {
  x <- rlang::sym(x)
  y <- rlang::sym(y)

  ggplot2::ggplot(trends, aes(x = !!x, y = !!y)) +
    ggplot2::geom_point(size = 0.1) +
    ggplot2::labs(
      x = "Date",
      y = "Search Volume"
    ) +
    ggplot2::theme_bw()
}
