test_that("multiplication draws matrix function works as expected", {
  expect_equal(gqIntMatrix(10, 1), as.matrix(qnorm(1:10 / 11)))
  expect_equal(gqIntMatrix(30, 2), matrix(c(qnorm(1:30 / 31), qnorm(1:30 / 31)), ncol = 2))
})
