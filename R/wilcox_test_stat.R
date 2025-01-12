wilcox_test_stat <- function(test, c, s) {
  W <- test$statistic

  # Jednovýběrový test - vše je v pořádku
  if (is.null(s$x2)) {
    n_non <- sum(s$x1 != c$H0)
    mu <- n_non * (n_non + 1) / 4
    s2 <- n_non * (n_non + 1) * (2 * n_non + 1) / 24
  } else {
    if (c$paired) {
      n_non <- sum((s$x1 - s$x2) != c$H0)
      mu <- n_non * (n_non + 1) / 4
      s2 <- n_non * (n_non + 1) * (2 * n_non + 1) / 24
    } else {
      mu <- (c$n * c$n) / 2
      s2 <- c$n * c$n * (c$n + c$n + 1) / 12
    }
  }

  return( (W - mu) / sqrt(s2) )
}