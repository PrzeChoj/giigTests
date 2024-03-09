test_that("partitions_to_base_matrices works", {
  p <- 5

  allVertexPartitions <- partitions::listParts(p) #podział na kolory wierzcholkow
  allEdges <- construct_edges(p)
  allEdgePartitions <- partitions::listParts(p*(p-1)/2) #podział na kolory krawędzi

  vPartition <- allVertexPartitions[[9]]
  ePartition <- allEdgePartitions[[1693]]

  my_BM <- partitions_to_base_matrices(vPartition, ePartition)
  testthat::expect_equal(is_Ishi_space(n, cumsum_n, matrix_list), TRUE)

  # No edges between colors of vertices
  ePartition <- list(ePartition[[2]], ePartition[[3]])
  my_BM <- partitions_to_base_matrices(vPartition, ePartition)
  testthat::expect_equal(is_Ishi_space(n, cumsum_n, matrix_list), TRUE)
})
