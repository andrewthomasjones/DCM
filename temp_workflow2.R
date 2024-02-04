library(DCM)
library(tictoc)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")
processedBWDCE2 <- join_choicedatasets(processedBW_rem, processedDCE)

#these should all give same result? the first two at leats

mtmm_emi_model1 <- loadEMIWorkbook(processedBWDCE2, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_18.xlsx")
mtmm_emi_model2 <- loadEMIWorkbook(processedBWDCE, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_19_zero.xlsx")

draw_sizes <- c(20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)

results <- list()
i <- 1

for(d in draw_sizes){

  print(paste0("Running model 1 for size = ", d))
  tic()
  mtmm1 <- runModel(mtmm_emi_model1, ghq_steps = d)
  time <- toc()

  temp <- list(size=d, model = "18", results = mtmm1$results, ll = mtmm1$loglikf, time = time)

  results[[i]] <- temp
  i <- i +1

  print(paste0("Running model 2 for size = ", d))
  tic()
  mtmm2 <- runModel(mtmm_emi_model2, ghq_steps = d)
  time2 <- toc()

  temp <- list(size=d, model = "19_zero", results = mtmm2$results, ll = mtmm2$loglikf, time = time2)

  results[[i]] <-  temp
  i <- i +1

  save(results, file = "C:/Projects/mtmm_performance_testing.RData")
  print(paste0("Saved results for size = ", d))
}
