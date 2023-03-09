#' @import tidyr
#'
#' @export
cor_keyword_specs <- function(trends) {
  trends %>%
    dplyr::group_by(ticker) %>%
    dplyr::arrange(ticker, keyword_spec, date) %>%
    dplyr::summarise(
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
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cor_df = score_matrix_to_cor_df(score_matrix) %>% list) %>%
    tidyr::unnest(cor_df)
}

score_matrix_to_cor_df <- function(score_matrix) {
  cors <- combn(colnames(score_matrix), 2)

  cor_matrix <- cor(score_matrix)

  purrr::map_dfr(seq_len(ncol(cors)), function(i) {
    tibble::tibble(
      cor_desc = paste(cors[1,i], cors[2,i], sep = "_"),
      cor = cor_matrix[cors[1,i], cors[2,i]]
    )
  })
}

#' @import ggplot2
#'
#' @export
plot_cors <- function(cor_df) {
  cor_df <- cor_df %>%
    dplyr::mutate(
      cor_desc = factor(
        cor_desc,
        levels = c(
          "stock_ticker",
          "stock_price_ticker",
          "company_name_ticker",
          "company_name_short_ticker",
          "stock_stock_price",
          "company_name_stock",
          "company_name_short_stock",
          "company_name_stock_price",
          "company_name_short_stock_price",
          "company_name_company_name_short"
        ) %>% rev
      )
    )

  ggplot2::ggplot(cor_df, mapping = aes(x = cor_desc, y = cor)) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(
      labels = c(
        stock_ticker = "<T> vs. <T> stock",
        stock_stock_price = "<T> stock vs. <T> stock price",
        stock_price_ticker = "<T> vs. <T> stock price",
        company_name_ticker = "<T> vs. <CN>",
        company_name_stock_price = "<T> stock price vs. <CN>",
        company_name_stock = "<T> stock vs. <CN>",
        company_name_short_ticker = "<T> vs. <CNS>",
        company_name_short_stock_price = "<T> stock price vs. <CNS>",
        company_name_short_stock = "<T> stock vs. <CNS>",
        company_name_company_name_short = "<CN> vs. <CNS>"
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1)
    ) +
    ggplot2::geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    ggplot2::labs(
      x = "",
      y = "Pearson Correlation Coefficient"
    ) +
    ggplot2::theme_bw()
}
