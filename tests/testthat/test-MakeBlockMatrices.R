test_that("partitions_to_base_matrices works", {
  p <- 5

  allVertexPartitions <- partitions::listParts(p) # podział na kolory wierzcholkow
  allEdges <- construct_edges(p)
  allEdgePartitions <- partitions::listParts(p * (p - 1) / 2) # podział na kolory krawędzi

  vPartition <- allVertexPartitions[[9]]
  ePartition <- allEdgePartitions[[1693]]

  testthat::expect_equal(is_Ishi_space(vPartition, ePartition), TRUE)

  # No edges between colors of vertices
  ePartition <- list(ePartition[[2]], ePartition[[3]])
  testthat::expect_equal(is_Ishi_space(vPartition, ePartition), TRUE)


  # EXP 1
  n <- c(2, 3, 1)
  cumsum_n <- cumsum(n)

  l_1_1 <- list(matrix(c(1, 0, 0, 1), nrow = n[1], byrow = TRUE), matrix(c(0, 1, 1, 0), nrow = n[1], byrow = TRUE))
  l_2_2 <- list(matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = n[2], byrow = TRUE), matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = n[2], byrow = TRUE))
  l_3_3 <- list(matrix(c(1), nrow = n[3], byrow = TRUE))
  l_2_1 <- list(matrix(c(1, 0, 1, 0, 0, 1), nrow = n[2], byrow = TRUE), matrix(c(0, 1, 0, 1, 1, 0), nrow = n[2], byrow = TRUE))
  l_3_1 <- list()
  l_3_2 <- list(matrix(c(1, 1, 1), nrow = n[3], byrow = TRUE))

  matrix_list <- list(list(l_1_1), list(l_2_1, l_2_2), list(l_3_1, l_3_2, l_3_3))

  vPartition <- list(c(1, 2), c(3, 4, 5), c(6))
  ePartition <- list(c(1), c(2, 3, 8), c(4, 6, 7), c(10, 11, 13), c(12, 14, 15))
  matrix_list2 <- partitions_to_base_matrices(vPartition, ePartition)

  testthat::expect_identical(matrix_list, matrix_list2)


  # EXP 2
  n <- c(4, 2, 1)
  cumsum_n <- cumsum(n)

  l_1_1 <- list(diag(n[1]), matrix(c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0), nrow = n[1], byrow = TRUE))
  l_2_2 <- list(diag(n[2]), matrix(c(0, 1, 1, 0), nrow = n[2], byrow = TRUE))
  l_3_3 <- list(diag(n[3]))
  l_2_1 <- list(matrix(c(1, 1, 0, 0, 0, 0, 1, 1), nrow = n[2], byrow = TRUE))
  l_3_1 <- list()
  l_3_2 <- list(matrix(c(1, 1), nrow = n[3], byrow = TRUE))

  matrix_list <- list(list(l_1_1), list(l_2_1, l_2_2), list(l_3_1, l_3_2, l_3_3))

  vPartition <- list(c(1, 2, 3, 4), c(5, 6), c(7))
  ePartition <- list(c(1, 12), c(4, 9, 14, 17), c(19), c(20, 21))
  matrix_list2 <- partitions_to_base_matrices(vPartition, ePartition)

  testthat::expect_identical(matrix_list, matrix_list2)
})
