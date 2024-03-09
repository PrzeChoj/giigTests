#' Is `el` the element of the `list`.
#'
#' @noRd
#'
#' @examples
#' is_in_list(c(1, 2, 3), list(c(1, 2, 3), c(3, 2, 1))) # TRUE
#' is_in_list(c(1, 2, 3), list(c(2, 3, 1), c(3, 2, 1))) # FALSE
is_in_list <- function(el, list) {
  return(any(sapply(list, function(x) identical(x, el))))
}

#' Change the partition from indexes of the edges to edges itself.
#'
#' @noRd
#'
#' @examples
#' p <- 4
#' edges <- construct_edges(p)
#' allPartitions <- partitions::listParts(p * (p - 1) / 2) # All possible colorings of edges
#' myPartition <- allPartitions[[7]] # this is the partition of indices of the edges
#' ECC <- PartitionToEdgeColorClasses(myPartition, edges) # this is the partition of the edges
PartitionToEdgeColorClasses <- function(partition, edges) {
  lapply(partition, function(p) edges[p])
}

#' Get all possible edges for a graph with p vertices
#'
#' @param p Number of vertices
#'
#' @noRd
#'
#' @example construct_edges(5) # 10 edges
construct_edges <- function(p) {
  pairs_list <- list()

  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      pairs_list <- c(pairs_list, list(c(i, j)))
    }
  }
  return(pairs_list)
}
