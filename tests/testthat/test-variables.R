

test_that("variables can be removed", {
  test_processed_set <- readRDS(test_path("fixtures", "test_processed_set.rds"))
  test_processed_set2 <- readRDS(test_path("fixtures", "test_processed_set2.rds"))

  select_set <- c("Safety_BW", "Reliability_BW","Comfort_BW", "Ease_of_Use_BW", "Convenience_BW",
                  "Efficiency_BW", "Information_BW", "Staff_helpfulness_BW", "Affordability_BW")

  expect_equal(remove_variables(test_processed_set, "Accessibility_BW"), test_processed_set2)

  expect_equal(select_variables(test_processed_set, select_set), test_processed_set2)
})


test_that("VIF wrapper behaves as expected", {
  test_processed_set2 <- readRDS(test_path("fixtures", "test_processed_set2.rds"))

  expect_equal(length(choice_model_vif(test_processed_set2)), 2)
  expect_equal(choice_model_vif(test_processed_set2)$VIF, c(1.8, 1.8, 1.8, 2.1, 1.8, 1.8, 1.8, 2.1, 2.1))
})
