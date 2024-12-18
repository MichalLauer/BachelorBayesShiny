get_sample <- function(d, c, i = 0) {
  if (c$use.seed) {
    set.seed(c$seed + i)
  }

  sample <- d$rand(c$n)

  return(sample)
}