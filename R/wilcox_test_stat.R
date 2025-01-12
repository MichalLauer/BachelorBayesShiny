wilcox_test_stat <- function(test, c, s) {
  W <- test$statistic

  # Jednovýběrový test - vše je v pořádku
  if (is.null(s$x2)) {
    mu <- c$n * (c$n + 1) / 4
    s2 <- c$n * (c$n + 1) * (2 * c$n + 1) / 24
    return((W - mu) / sqrt(s2))
  } else {
    if (c$paired) {
      # Párový test - je nutné upravit W
      # R používá minimum z W a n(n+1)/2 - W
      W <- min(W, (c$n * (c$n + 1) / 2) - W)
      mu <- c$n * (c$n + 1) / 4
      s2 <- c$n * (c$n + 1) * (2 * c$n + 1) / 24
      return((W - mu) / sqrt(s2))
    } else {
      mu <- c$n * (c$n + c$n + 1) / 2
      s2 <- c$n * c$n * (c$n + c$n + 1) / 12

      # Vypočtení správné statistiky pro normalitu
      combined <- c(s$x1, s$x2)
      ranks <- rank(combined)
      W <- sum(ranks[1:c$n])

      return((W - mu) / sqrt(s2))
    }
  }
}