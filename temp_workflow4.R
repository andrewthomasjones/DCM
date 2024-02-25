library(DCM)
#library(tictoc)
processedDCE <- setUp(DCEpriorities)
#[DCEpriorities$ID < 1003, 1:4]
model_fixed <- model_generator(processedDCE, "fixed")
# model_random <- model_generator(processedDCE, "random")
# model_1f <- model_generator(processedDCE, "one-factor")
#
# processedBW <- setUp(BWpriorities)
# processedBWDCE <- join_choicedatasets(processedBW, processedDCE)
#
# model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")

#c code
test_fixed_ghq <- runModel(model_fixed, verbose = 2)
test_fixed_ghq2 <- runModel(model_fixed,  dev_mode = "ghq2", verbose = 2)
test_fixed_ghq$LL
test_fixed_ghq2$LL


test_fixed_draws <- runModel(model_fixed,  dev_mode = "draws")

#R code
test_fixed_orig <- runModel(model_fixed,  dev_mode = "orig")
test_fixed_ghq2 <- runModel(model_fixed,  dev_mode = "ghq2")
#new with draws


test_fixed_ghq$LL
test_fixed_draws$LL

test_fixed_orig$LL
test_fixed_ghq2$LL







test_random_ghq <- runModel(model_random, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_random_orig <- runModel(model_random, dev_mode = "orig", verbose = 2)

test_1f_ghq <- runModel(model_1f, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_1f_orig <- runModel(model_1f, dev_mode = "orig",  verbose = 2)

test_mtmm_ghq <- runModel(model_mtmm, dev_mode = "ghq", ghq_size = 3, verbose = 2)
test_mtmm_orig <- runModel(model_mtmm, dev_mode = "orig", verbose = 2)

