library(DCM)
#library(tictoc)
processedDCE <- setUp(DCEpriorities)
processedBW <- setUp(BWpriorities)
processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

model_fixed <- model_generator(processedBW, "fixed")
model_random <- model_generator(processedBW, "random")
model_1f <- model_generator(processedBW, "one-factor")

model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")

#c code
test_fixed_ghq <- runModel(model_fixed, dev_mode = "C", verbose = 2)
test_fixed_ghq2 <- runModel(model_fixed,  dev_mode = "R", verbose = 2)
test_fixed_ghq$LL
test_fixed_ghq2$LL

test_random_ghq <- runModel(model_random, dev_mode = "C", ghq_size = 3, verbose = 2)
test_random_ghq2 <- runModel(model_random, dev_mode = "R", ghq_size = 3, verbose = 2)
test_random_ghq$LL
test_random_ghq2$LL

test_1f_ghq <- runModel(model_1f, dev_mode = "C", ghq_size = 3, verbose = 2)
test_1f_ghq2 <- runModel(model_1f, dev_mode = "R", ghq_size = 3, verbose = 2)

test_1f_ghq$LL
test_1f_ghq2$LL

test_mtmm_ghq <- runModel(model_mtmm, dev_mode = "C", ghq_size = 3, verbose = 2)
test_mtmm_ghq2 <- runModel(model_mtmm, dev_mode = "R", ghq_size = 3, verbose = 2)
test_mtmm_ghq$LL
test_mtmm_ghq2$LL
