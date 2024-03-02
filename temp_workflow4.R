library(DCM)
library(tictoc)

#full sample
processedDCE <- setUp(DCEpriorities)
processedBW <- setUp(BWpriorities)


#reduce sample size to increase speed
# processedDCE <- setUp(DCEpriorities[DCEpriorities$ID < 1030, ])
# processedBW <- setUp(BWpriorities[BWpriorities$ID < 1030, ])
processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")


processedBWDCEr <- join_choicedatasets(processedBW_rem, processedDCE)
processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

model_fixed <- model_generator(processedBWDCEr, "fixed")
model_random <- model_generator(processedBWDCEr, "random")
model_1f <- model_generator(processedBWDCEr, "one-factor")

model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")

tic()
test_fixed_ghq <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 0) #16.198 sec elapsed 3
toc()
# tic()
# test_fixed_ghq2 <- runModel(model_fixed,  dev_mode = "R",  verbose = 0) #32.637 sec elapsed
# toc()
test_fixed_ghq$LL
#15999.41 3
#15999.41 4

tic()
test_random_ghq <- runModel(model_random, dev_mode = "C",  ghq_size = 3, verbose = 0) #1826.179 sec elapsed 3 (0:30hrs)
toc()
# tic()
# test_random_ghq2 <- runModel(model_random, dev_mode = "R", ghq_size = 3, verbose = 0) # > 529.732 sec elapsed
# toc()
test_random_ghq$LL
#14330.16 3
#13984.31 4 27 hours

tic()
test_1f_ghq <- runModel(model_1f, dev_mode = "C",  ghq_size = 3, verbose = 0) #74.163 sec elapsed 3 5899.899 sec elapsed 4
toc()
# tic()
# test_1f_ghq2 <- runModel(model_1f, dev_mode = "R", ghq_size = 3, verbose = 0) #133.59 sec elapsed
# toc()
test_1f_ghq$LL
#15247.67 3
#15211.1 4


tic()
test_mtmm_ghq <- runModel(model_mtmm, dev_mode = "C", ghq_size = 3, verbose = 2) #13577.193 sec elapsed 3 (3:45hrs)
toc()
test_mtmm_ghq$LL
#13465.76 3


