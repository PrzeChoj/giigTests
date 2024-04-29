#' Change calculate the factor graph and make the graph for nauty
#'
#' nauty numbers vertices from 0, not from 1.
#'
#' @export
#'
#' @examples
#' vPartition <- list(c(1, 2), c(3, 4))
#' ePartition <- list(1, c(3, 4), 6)
#'
#' make_factor_graph_for_nauty(vPartition, ePartition)
#'
#' vPartition <- list(c(1, 2, 3, 4))
#' ePartition <- list(c(1, 3, 4, 6), c(2, 5))
#'
#' make_factor_graph_for_nauty(vPartition, ePartition)
make_factor_graph_for_nauty <- function(vPartition, ePartition) {
  p <- sum(sapply(vPartition, length))
  num_edges <- sum(sapply(ePartition, length))
  new_p <- p + num_edges

  all_edges <- unlist(ePartition)
  inv_all_edges <- rep(NA, max(all_edges))
  inv_all_edges[all_edges] <- seq_along(all_edges)

  new_ePartition <- lapply(ePartition, function(edges) {
    inv_all_edges[edges] + p
  })
  factor_vPartition <- c(vPartition, new_ePartition)

  # Change vertices numeration from starting with 1 to starting with 0:
  factor_vPartition <- sapply(factor_vPartition, function(x){min(x) - 1})

  # get edges:
  factor_edges_from <- numeric(length(all_edges) * 2)
  factor_edges_to <- numeric(length(all_edges) * 2)
  for (edge_number in 1:length(all_edges)) {
    ij <- get_edge_of_a_number(p, all_edges[edge_number])
    factor_edges_from[edge_number * 2 - 1] <- ij[1]
    factor_edges_to[edge_number * 2 - 1] <- edge_number + p
    factor_edges_from[edge_number * 2] <- ij[2]
    factor_edges_to[edge_number * 2] <- edge_number + p
  }

  # Change vertices numeration from starting with 1 to starting with 0:
  factor_edges_from <- factor_edges_from - 1
  factor_edges_to <- factor_edges_to - 1

  list(
    new_p,
    factor_vPartition,
    factor_edges_from,
    factor_edges_to
  )
}
