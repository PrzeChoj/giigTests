test_that("make_factor_graph works", {
  p <- 4
  vPartition <- list(c(1, 2), c(3, 4))
  ePartition <- list(1, c(3, 4), 6)

  my_factor_graph <- list(
    c(0, 1, 2, 3, 4, 5, 6, 7),
    c(1, 0, 1, 0, 0, 1, 0, 0),
    c(0, 1, 0, 3, 1, 2, 2, 3),
    c(4, 4, 5, 5, 6, 6, 7, 7)
  )

  expect_equal(make_factor_graph_for_nauty(vPartition, ePartition), my_factor_graph)
})
