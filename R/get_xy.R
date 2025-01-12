get_xy <- function(d) {
  x <- seq(from = d$quantile(1/1000),
            to = d$quantile(1 - (1/1000)),
            length.out = 1000)
  y <- d$pdf(x)
  n <- d$strprint()

  return(list(x = x, y = y, n = n))
}