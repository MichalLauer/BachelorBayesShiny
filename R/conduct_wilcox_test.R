conduct_wilcox_test <- function(x, y, c, use_h0 = TRUE) {
  wilcox.test(x = x, y = y,
              mu = if (use_h0) c$H0 else c$H1,
              paired = c$paired,
              correct = FALSE,
              exact = FALSE)
}