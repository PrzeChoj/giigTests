test_that("Ishi test works", {
  # EXP1, NON-Ishi
  n <- c(2, 3, 1)
  cumsum_n <- cumsum(n)

  vPartition <- list(c(1, 2), c(3, 4, 5), c(6))
  ePartition <- list(c(1), c(2, 3, 8), c(4, 6, 7), c(10, 11, 13), c(12, 14, 15))
  matrix_list <- partitions_to_base_matrices(vPartition, ePartition)

  # (M0)
  testthat::expect_equal(is_M0_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M1)
  testthat::expect_equal(is_M1_condition_satisfied(n, cumsum_n, matrix_list), FALSE)

  # (M2)
  testthat::expect_equal(is_M2_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # All three
  testthat::expect_equal(is_Ishi_space(vPartition, ePartition), FALSE)

  # EXP2, Ishi
  n <- c(4, 2, 1)
  cumsum_n <- cumsum(n)

  vPartition <- list(c(1, 2, 3, 4), c(5, 6), c(7))
  ePartition <- list(c(1, 12), c(4, 9, 14, 17), c(19), c(20, 21))
  matrix_list <- partitions_to_base_matrices(vPartition, ePartition)

  # (M0)
  testthat::expect_equal(is_M0_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M1)
  testthat::expect_equal(is_M1_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # (M2)
  testthat::expect_equal(is_M2_condition_satisfied(n, cumsum_n, matrix_list), TRUE)

  # All three
  testthat::expect_equal(is_Ishi_space(vPartition, ePartition), TRUE)



  # not Ishi
  p <- 16

  vPartition <- list(1:16)
  ePartition <- list(
    c(1, 30, 76, 55, 93, 106, 117, 118),
    c(3, 16, 57, 66, 95, 100, 115, 120),
    c(2, 17, 67, 56, 94, 101, 116, 119),
    c(18, 34, 45, 5, 110, 102, 113, 97),
    c(7, 20, 32, 43, 99, 104, 108, 111),
    c(6, 4, 21, 19, 31, 33, 44, 46, 96, 98, 105, 103, 107, 109, 114, 112)
  )

  testthat::skip("TODO: investigate")
  testthat::expect_equal(is_Ishi_space(vPartition, ePartition), FALSE)
})
