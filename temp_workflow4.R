library(DCM)
#library(tictoc)

processedDCE <- setUp(DCEpriorities)

model_fixed <- model_generator(processedDCE, "fixed")
model_random <- model_generator(processedDCE, "random")
model_1f <- model_generator(processedDCE, "one-factor")

processedBW <- setUp(BWpriorities)
processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")


test_fixed_ghq <- runModel(model_fixed, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_fixed_orig <- runModel(model_fixed, dev_mode = "orig", verbose = 2)

test_random_ghq <- runModel(model_random, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_random_orig <- runModel(model_random, dev_mode = "orig", verbose = 2)

test_1f_ghq <- runModel(model_1f, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_1f_orig <- runModel(model_1f, dev_mode = "orig",  verbose = 2)

test_mtmm_ghq <- runModel(model_mtmm, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_mtmm_orig <- runModel(model_mtmm, dev_mode = "orig", verbose = 2)

