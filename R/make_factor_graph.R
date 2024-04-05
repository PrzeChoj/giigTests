make_factor_graph <- function(vPartition, ePartition) {
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

  factor_edges <- numeric(0)
  for (edge_number in 1:length(all_edges)) {
    ij <- get_edge_of_a_number(p, edge_number)
    factor_edges <- c(
      factor_edges,
      get_number_of_an_edge(new_p, ij[1], edge_number + p),
      get_number_of_an_edge(new_p, ij[2], edge_number + p)
    )
  }

  list(
    factor_vPartition,
    factor_edges,
    all_edges
  )
}
