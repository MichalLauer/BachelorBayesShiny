conduct_wilcox_test <- function(x, y, c, h0 = NULL) {
  wilcox.test(x = x, y = y,
              mu = h0 %||% c$H0,
              paired = c$paired,
              correct = FALSE,
              exact = FALSE)
}