library(DCM)
library(tictoc)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")
processedBWDCE2 <- join_choicedatasets(processedBW_rem, processedDCE)
processedBWDCE <- join_choicedatasets(processedBW, processedDCE)


load("/Users/uqajon14/Downloads/BWprioritiesDCEpriorities.RData")

processedBWDCE_old <- list()

processedBWDCE_old$data <- processed$data
processedBWDCE_old$ndecisionmakers <- processed$ndecisionmakers
processedBWDCE_old$concept <- processed$concept
processedBWDCE_old$fdd <- processed$fdd
processedBWDCE_old$attribute_names <- processed$attributenames
processedBWDCE_old$ncovariates <- processed$ncovariates
processedBWDCE_old$nmax_choiceset_size <-  processed$nmaxchoicesetsize

#these arent direct copy but as close as possible
processedBWDCE_old$data_name <- "convert over old data from Tom"
processedBWDCE_old$npp <- processed$ncovariates #19
processedBWDCE_old$lcovariates <- paste("Cov", 1:processedBWDCE_old$npp)

createEMIWorkbook(processedBWDCE2,  "mtmm", "~/Desktop/new2")

#manual edits
#new2/EMI_mtmm_init.xlsx is new2/EMI_mtmm.xlsx with init values from EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx
#new/EMI_mtmm_manual.xls is new2/EMI_mtmm.xlsx with manual zeroing (which actually means adding rows)


#old data (as close as possible), old emi
m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "~/Desktop/old/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
test_old <- runModel(m1BWDCE_EMI_old)

#new data (19), old emi
m1BWDCE_EMI_mix <- loadEMIWorkbook(processedBWDCE, "~/Desktop/old/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
test_mix <- runModel(m1BWDCE_EMI_mix)

#new data (18), new emi
m1BWDCE_EMI_new2 <- loadEMIWorkbook(processedBWDCE2, "~/Desktop/new2/EMI_mtmm.xlsx")
test_new <- runModel(m1BWDCE_EMI_new2)

#new data (19), new emi (manually remove as in old emi)
m1BWDCE_EMI_mix2 <- loadEMIWorkbook(processedBWDCE, "~/Desktop/new/EMI_mtmm_manual.xlsx")
test_mix2 <- runModel(m1BWDCE_EMI_mix2)

#old data, new emi (manually remove as in old emi)
m1BWDCE_EMI_mix3 <- loadEMIWorkbook(processedBWDCE_old, "~/Desktop/new/EMI_mtmm_manual.xlsx")
test_mix3 <- runModel(m1BWDCE_EMI_mix3)

#new data, 18 vars, new EMI but with old init values
m1BWDCE_EMI_new_init <- loadEMIWorkbook(processedBWDCE2, "~/Desktop/new2/EMI_mtmm_init.xlsx")
test_new_old_inits <- runModel(m1BWDCE_EMI_new_init)

test_new$results
test_old$results
test_mix$results
test_mix2$results
test_mix3$results
test_new_old_inits$results
read.csv("/Users/uqajon14/Downloads/RESULTS_BWprioritiesDCEpriorities_mtmm1000 (1).csv")

#these should all give same result? the first two at leats

#mtmm_emi_model1 <- loadEMIWorkbook(processedBWDCE2, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_18.xlsx")
#mtmm_emi_model2 <- loadEMIWorkbook(processedBWDCE, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_19_zero.xlsx")

#draw_sizes <- c(20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)

#results <- list()
#i <- 1
#
#for(d in draw_sizes){
#
#  print(paste0("Running model 1 for size = ", d))
#  tic()
#  mtmm1 <- runModel(mtmm_emi_model1, ghq_steps = d)
#  time <- toc()
#
#  temp <- list(size=d, model = "18", results = mtmm1$results, ll = mtmm1$loglikf, time = time)
#
#  results[[i]] <- temp
#  i <- i +1
#
#  print(paste0("Running model 2 for size = ", d))
#  tic()
#  mtmm2 <- runModel(mtmm_emi_model2, ghq_steps = d)
#  time2 <- toc()
#
#  temp <- list(size=d, model = "19_zero", results = mtmm2$results, ll = mtmm2$loglikf, time = time2)
#
#  results[[i]] <-  temp
#  i <- i +1
#
#  save(results, file = "C:/Projects/mtmm_performance_testing.RData")
#  print(paste0("Saved results for size = ", d))
#}

