test_that("frequency distribution function works", {
  test_processed_set <- readRDS(test_path("fixtures", "test_processed_set.rds"))
  expect_equal(min(frequencyDistribution(test_processed_set)[,1]),1001)
  expect_equal(max(frequencyDistribution(test_processed_set)[,1]),1308)
  expect_equal(nrow(frequencyDistribution(test_processed_set)),308)
  expect_equal(frequencyDistribution(test_processed_set)[,2],rep(20,308))
})


test_that("direct C++ frequency distribution function works", {
  test_processed_set <- readRDS(test_path("fixtures", "test_processed_set.rds"))
  res <- DCM:::frequencyDistributionCpp(matrix(test_processed_set$data[, 1], length(test_processed_set$data[, 1]), 1))
  expect_equal(min(res[,1]),1001)
  expect_equal(max(res[,1]),1308)
  expect_equal(nrow(res),308)
  expect_equal(res[,2],rep(20,308))
})

