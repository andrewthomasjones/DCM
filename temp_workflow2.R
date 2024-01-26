library(DCM)

processedBW<-setUp(BWpriorities)
processedDCE<-setUp(DCEpriorities)
processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")
processedBWDCE2 <- join_choicedatasets(processedBW_rem, processedDCE)

m1BWDCE<-model_generator(processedBWDCE2, "one-factor")

test <- runModel(m1BWDCE)
test$results


processedBWDCE <- join_choicedatasets(processedBW, processedDCE)
createEMIWorkbook(processedBWDCE,  "one-factor", "~/Desktop")
#edit here
m1BWDCE_EMI <- loadEMIWorkbook(processedBWDCE, "~/Desktop/EMI_one-factor.xlsx")

test2 <- runModel(m1BWDCE_EMI)
test2$results
