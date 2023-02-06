#' @export
cor_keyword_specs <- function(trends) {
  trends %>%
    group_by(ticker) %>%
    arrange(ticker, keyword_spec, date) %>%
    summarise(
      score_matrix = list(
        matrix(
          score,
          ncol = length(unique(keyword_spec)),
          dimnames = list(
            unique(as.character(date)),
            unique(keyword_spec)
          )
        )
      )
    )
}
