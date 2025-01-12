is_second_valid <- function(x, d2) {
  if (!x) {
    return(NULL)
  } else if (d2 == "") {
    return("Je nutné zadat druhé rozdělení")
  } else {
    return(is_distr(d2))
  }
}