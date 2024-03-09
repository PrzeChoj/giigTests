test_that("Ishi test works", {
  # EXP1, NON-Ishi
  n <- c(2,3,1)
  cumsum_n <- cumsum(n)

  l_1_1 <- list(matrix(c(1,0,0,1), nrow = n[1], byrow = TRUE), matrix(c(0,1,1,0), nrow = n[1], byrow = TRUE))
  l_2_2 <- list(matrix(c(1,0,0,0,1,0,0,0,1), nrow = n[2], byrow = TRUE), matrix(c(0,1,1,1,0,1,1,1,0), nrow = n[2], byrow = TRUE))
  l_3_3 <- list(matrix(c(1), nrow = n[3], byrow = TRUE))
  l_2_1 <- list(matrix(c(1,0,1,0,0,1), nrow = n[2], byrow = TRUE), matrix(c(0,1,0,1,1,0), nrow = n[2], byrow = TRUE))
  l_3_1 <- list()
  l_3_2 <- list(matrix(c(1,1,1), nrow = n[3], byrow = TRUE))

  matrix_list <- list(list(l_1_1), list(l_2_1, l_2_2), list(l_3_1, l_3_2, l_3_3))

  # (M0)
  testthat::expect_equal(is_M0_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M1)
  testthat::expect_equal(is_M1_condition_satisfied(n, cumsum_n, matrix_list), FALSE)

  # (M2)
  testthat::expect_equal(is_M2_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # All three
  testthat::expect_equal(is_Ishi_space(n, cumsum_n, matrix_list), FALSE)

  # EXP2, Ishi
  n <- c(4,2,1)
  cumsum_n <- cumsum(n)

  l_1_1 <- list(diag(n[1]), matrix(c(0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0), nrow = n[1], byrow = TRUE))
  l_2_2 <- list(diag(n[2]), matrix(c(0,1,1,0), nrow = n[2], byrow = TRUE))
  l_3_3 <- list(diag(n[3]))
  l_2_1 <- list(matrix(c(1,1,0,0,0,0,1,1), nrow = n[2], byrow = TRUE))
  l_3_1 <- list()
  l_3_2 <- list(matrix(c(1,1), nrow = n[3], byrow = TRUE))

  matrix_list <- list(list(l_1_1), list(l_2_1, l_2_2), list(l_3_1, l_3_2, l_3_3))

  # (M0)
  testthat::expect_equal(is_M0_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M1)
  testthat::expect_equal(is_M1_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M2)
  testthat::expect_equal(is_M2_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # All three
  testthat::expect_equal(is_Ishi_space(n, cumsum_n, matrix_list), TRUE)



  # not Ishi
  p <- 16

  vPartition <- list(1:16)
  ePartition <- list(
    c(1,30,76,55,93,106,117,118),
    c(3,16,57,66,95,100,115,120),
    c(2,17,67,56,94,101,116,119),
    c(18,34,45,5,110,102,113,97),
    c(7,20,32,43,99,104,108,111),
    c(6,4,21,19,31,33,44,46,96,98,105,103,107,109,114,112)
  )

  testthat::expect_equal(is_Ishi_space(p, p, partitions_to_base_matrices(vPartition, ePartition)), FALSE)
})
