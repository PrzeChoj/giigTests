get_number_of_an_edge <- function(p, i, j) {
  if (p < max(i, j)) {
    stop("Wrong i or j")
  }
  if (i >= j) {
    stop("i >= j")
  }

  n_before_i(p, i) + (j - i)
}

# I think the get_edge_of_a_number can be made faster with floor or sth...
get_edge_of_a_number <- function(p, n) {
  if (n > choose(p, 2)) {
    stop("Too big n")
  }
  if (n < 1) {
    stop("n < 1")
  }

  my_i <- 1
  while (n_before_i(p, my_i) < n) {
    my_i <- my_i + 1
  }
  my_i <- my_i - 1

  my_j <- n - n_before_i(p, my_i) + my_i

  c(my_i, my_j)
}

n_before_i <- function(p, i) {
  (i - 1) * p - (i - 1) * i / 2
}
