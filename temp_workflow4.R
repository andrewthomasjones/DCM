library(DCM)
library(tictoc)

#full sample
processedDCE <- setUp(DCEpriorities)
processedBW <- setUp(BWpriorities)


#reduce sample size to increase speed
processedDCE <- setUp(DCEpriorities[DCEpriorities$ID < 1030, ])
processedBW <- setUp(BWpriorities[BWpriorities$ID < 1030, ])


processedBWDCE <- join_choicedatasets(processedBW, processedDCE)


model_fixed <- model_generator(processedBW, "fixed")
model_random <- model_generator(processedBW, "random")
model_1f <- model_generator(processedBW, "one-factor")

model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")

tic()
test_fixed_ghq <- runModel(model_fixed,  dev_mode = "C",  verbose = 0) #1.839 sec elapsed
toc()
tic()
test_fixed_ghq2 <- runModel(model_fixed,  dev_mode = "R",  verbose = 0) #32.637 sec elapsed
toc()
test_fixed_ghq$LL
test_fixed_ghq2$LL #agree


tic()
test_random_ghq <- runModel(model_random, dev_mode = "C",  ghq_size = 3, verbose = 0) #39.892 sec elapsed
toc()
tic()
test_random_ghq2 <- runModel(model_random, dev_mode = "R", ghq_size = 3, verbose = 0) # > 529.732 sec elapsed
toc()
test_random_ghq$LL
test_random_ghq2$LL #agree


tic()
test_1f_ghq <- runModel(model_1f, dev_mode = "C",  ghq_size = 3, verbose = 0) #11.087 sec elapsed
toc()
tic()
test_1f_ghq2 <- runModel(model_1f, dev_mode = "R", ghq_size = 3, verbose = 0) #133.59 sec elapsed
toc()
test_1f_ghq$LL
test_1f_ghq2$LL #agree



tic()
test_mtmm_ghq <- runModel(model_mtmm, dev_mode = "C", ghq_size = 3, verbose = 1)
toc()
tic()
test_mtmm_ghq2 <- runModel(model_mtmm, dev_mode = "R", ghq_size = 3, verbose = 1)
toc()
test_mtmm_ghq$LL
test_mtmm_ghq2$LL #agree


