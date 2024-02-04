library(DCM)

processedBW<-setUp(BWpriorities)
processedDCE<-setUp(DCEpriorities)
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


processedBWDCE_old$data_name <- "convert over old data from Tom"

processedBWDCE_old$lcovariates <- processedBWDCE2$lcovariates
processedBWDCE_old$npp <- processedBWDCE2$npp

createEMIWorkbook(processedBWDCE2,  "mtmm", "~/Desktop/new2")

m1BWDCE_EMI_new2 <- loadEMIWorkbook(processedBWDCE2, "~/Desktop/new2/EMI_mtmm.xlsx")
test_new <- runModel(m1BWDCE_EMI_new2)

m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "~/Desktop/old/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
test_old <- runModel(m1BWDCE_EMI_old)

m1BWDCE_EMI_mix <- loadEMIWorkbook(processedBWDCE, "~/Desktop/old/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
test_mix <- runModel(m1BWDCE_EMI_mix)

test_new$results
test_old$results
test_mix$results
