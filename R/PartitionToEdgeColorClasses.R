PartitionToEdgeColorClasses <- function(partition, edges) {
  lapply(partition, function(p) edges[p])
}
