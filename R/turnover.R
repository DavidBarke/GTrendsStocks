#' Detrend time-series as outlined in Chordia et al. (2007)
#'
#' @export
detrend_chordia_2007 <- function(turnover, month, t) {
  df <- tibble::tibble(
    turnover = turnover,
    month = month,
    t = t
  ) %>%
    dplyr::mutate(
      t2 = t^2
    )

  # Equation (2)
  lm_eq_2 <- lm(turnover ~ factor(month) + t + t2, df)

  # Residuals xi
  df$xi <- lm_eq_2$residuals

  # Equation (3)
  lm_eq_3 <- lm(log(xi^2) ~ factor(month) + t + t2, df)

  # Predict log(xi^2)
  df$eq_3_pred <- predict(lm_eq_3, df)

  term <- df$xi / exp(df$eq_3_pred / 2)
  lambda <- sqrt(var(df$turnover) / var(term))
  alpha <- mean(df$turnover) - lambda * mean(term)

  df$abn_turnover <- alpha + lambda * term

  df$abn_turnover
}
