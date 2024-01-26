test_data_matrix <- as.data.frame(BWpriorities)
saveRDS(test_data_matrix, file="./tests/testthat/fixtures/test_data_matrix.rds")

test_concepts <- createConcepts(test_data_matrix, nmax_choiceset_size = 31)
saveRDS(test_concepts, file=".tests/testthat/fixtures/test_concepts.rds")

test_processed_set <- setUp(BWpriorities)
saveRDS(test_processed_set, file="./tests/testthat/fixtures/test_processed_set.rds")

test_processed_set2  <- remove_variables(test_processed_set, "Accessibility_BW")
saveRDS(test_processed_set2, file="./DCM/tests/testthat/fixtures/test_processed_set2.rds")

