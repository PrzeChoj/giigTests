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
})
