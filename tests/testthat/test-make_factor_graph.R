test_that("make_factor_graph works", {
  p <- 4
  vPartition <- list(c(1, 2), c(3, 4))
  ePartition <- list(1, c(3, 4), 6)

  my_factor_graph <- list(
    list(c(1, 2), c(3, 4), 5L, 6:7, 8L),
    c(4, 10, 5, 16, 6, 21, 13, 18),
    c(1, 3, 4, 6)
  )
  expect_equal(make_factor_graph(vPartition, ePartition), my_factor_graph)
})
