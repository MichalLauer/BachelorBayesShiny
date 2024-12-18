conduct_t_test <- function(x, y, c, h0 = NULL) {
  t.test(x = x, y = y,
         mu = h0 %||% c$H0,
         paired = c$paired,
         var.equal = c$var.equal)
}