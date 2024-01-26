test_that("createConcepts works correctly", {
  test_data_matrix <- readRDS(test_path("fixtures", "test_data_matrix.rds"))
  test_concepts <- readRDS(test_path("fixtures", "test_concepts.rds"))
  expect_equal(createConcepts(test_data_matrix, nmax_choiceset_size = 31), test_concepts)
  # should throw error if max size is too small
  expect_error(createConcepts(test_data_matrix, nmax_choiceset_size = 11))
})

test_that("nmax_choiceset_size is correct", {
  test_data_matrix <- readRDS(test_path("fixtures", "test_data_matrix.rds"))
  expect_equal(max(unlist(rle(test_data_matrix [, 2])[1])), 12)
})


