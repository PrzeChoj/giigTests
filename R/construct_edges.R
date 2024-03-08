construct_edges <- function(p) {
  pairs_list <- list()

  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      pairs_list <- c(pairs_list, list(c(i, j)))
    }
  }
  return(pairs_list)
}
