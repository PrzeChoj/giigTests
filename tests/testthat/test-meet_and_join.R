test_that("meet and join works", {
  v1 <- list(c(1,2,3),c(4))
  e1 <- list(c(1,2,4),c(3),c(5,6))
  v2 <- list(c(1,2),c(3),c(4))
  e2 <- list(c(1),c(2,4),c(3,5),c(6))
  is_Ishi_space(v1,e1) # TRUE
  is_Ishi_space(v2,e2) # TRUE

  skip("Check output by hand")
  meet_output <- meet(v1,e1,v2,e2)
  meet_should_be <- list(list(c(1, 2, 3), 4),
                         list(c(1, 2, 4), c(3, 5, 6)))

  expect_equal(meet_output, meet_should_be)

  skip("Check output by hand")
  join_output <- join(v1,e1,v2,e2)
  join_should_be <- list(list(4, 1, 2, 3),
                         list(1, 2, 4, 3, 5, 6))

  expect_equal(join_output, join_should_be)
})
