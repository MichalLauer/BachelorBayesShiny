conduct_t_test <- function(x, y, c, use_h0 = TRUE) {
  t.test(x = x, y = y,
         mu = if (use_h0) c$H0 else c$H1,
         paired = c$paired,
         var.equal = c$var.equal)
}