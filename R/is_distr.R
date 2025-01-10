is_distr <- function(x) {
  if (x == "") {
    return(NULL)
  }
  tryCatch({
    dparse(x)
    return(NULL)
  }, error = function(err) {
    glue("'<<x>>' není validní {distr6} rozdělení.",
         .open = "<<", .close = ">>")
  }
  )
}