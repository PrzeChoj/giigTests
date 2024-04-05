test_that("get_number_of_an_edge works", {
  expect_equal(get_number_of_an_edge(4, 2, 4), 5)
  expect_equal(get_number_of_an_edge(4, 3, 4), 6)

  expect_equal(get_edge_of_a_number(4, 2), c(1, 3))
  expect_equal(get_edge_of_a_number(4, 4), c(2, 3))

  p <- 10
  n <- 38
  ij <- get_edge_of_a_number(p, n)
  n_again <- get_number_of_an_edge(p, ij[1], ij[2])
  expect_equal(n, n_again)
})
