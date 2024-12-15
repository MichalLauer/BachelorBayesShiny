get_sample <- function(d, n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  sample <- d$rand(n)

  return(sample)
}