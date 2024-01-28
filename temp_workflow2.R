library(DCM)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")
processedBWDCE2 <- join_choicedatasets(processedBW_rem, processedDCE)

#these should all give same result? the first two at leats


mtmm_emi_model1 <- loadEMIWorkbook(processedBWDCE2, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_18.xlsx")

mtmm1 <- runModel(mtmm_emi_model1)


mtmm_emi_model2 <- loadEMIWorkbook(processedBWDCE, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_19_zero.xlsx")

mtmm2 <- runModel(mtmm_emi_model2)


mtmm_emi_model3 <- loadEMIWorkbook(processedBWDCE, "C:/Projects/EMI_BWprioritiesDCEpriorities_MTMM_19_blank.xlsx")

mtmm3 <- runModel(mtmm_emi_model3)
