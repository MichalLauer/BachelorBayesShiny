wilcox_test_stat <- function(test, c, s) {
  if (is.null(s$x2)) {
    W <- test$statistic
    mu <- c$n*(c$n + 1)/4
    s2 <- c$n*(c$n + 1)*(2*c$n + 1)/24
  } else {
    if (c$paired) {
      d <- s$x1 - s$x2
      d_nonzero <- d[d != 0]
      n <- length(d_nonzero)
      ranks <- rank(abs(d_nonzero))
      signed_ranks <- ranks * sign(d_nonzero)
      W <- sum(signed_ranks[signed_ranks > 0])
      mu <- c$n * (c$n + 1) / 4
      s2 <- c$n * (c$n + 1) * (2 * c$n + 1) / 24
    } else {
      W <- test$statistic + c$n * (c$n + 1) / 2
      mu <- c$n*(2*c$n + 1)/2
      s2 <- c$n*c$n*(2*c$n + 1)/12
    }
  }

  (W - mu) / sqrt(s2)
}