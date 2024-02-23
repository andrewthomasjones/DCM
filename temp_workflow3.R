#library(DCM)
#library(tictoc)

# df <- data.frame(matrix(NA, nrow=1, ncol=63))
# colnames(df) <- paste0("V_", 1:63)
# empty_df = df[FALSE,]
# write.csv(empty_df, "./TESTING_DUMP/new_working_values.csv")


# processedBW <- setUp(BWpriorities)
# processedDCE <- setUp(DCEpriorities)
# processedBWDCE <- join_choicedatasets(processedBW, processedDCE)
# #
# load("./TESTING_DUMP/BWprioritiesDCEpriorities.RData")
#
# processedBWDCE_old <- list()
#
# processedBWDCE_old$data <- processed$data
# processedBWDCE_old$ndecisionmakers <- processed$ndecisionmakers
# processedBWDCE_old$concept <- processed$concept
# processedBWDCE_old$fdd <- processed$fdd
# processedBWDCE_old$attribute_names <- processed$attributenames
# processedBWDCE_old$ncovariates <- processed$ncovariates
# processedBWDCE_old$nmax_choiceset_size <-  processed$nmaxchoicesetsize
#
# #these arent direct copy but as close as possible
# processedBWDCE_old$data_name <- "convert over old data from Tom"
# processedBWDCE_old$npp <- processed$ncovariates #19
# processedBWDCE_old$lcovariates <- paste("Cov", 1:processedBWDCE_old$npp)

# ###############################################
# createEMIWorkbook(processedBWDCE,  "fixed", working_folder = "TESTING_DUMP")
#
# m1BWDCE_EMI_new_fix <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_fixed.xlsx")
# test_new_fix <- runModel(m1BWDCE_EMI_new_fix, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
# m1BWDCE_EMI_old_fix <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_fixed.xlsx")
# test_old_fix <- runModel(m1BWDCE_EMI_old_fix, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
# ###############################################

#m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")



# ###############################################
# m_1f <-model_generator(processedDCE, "one-factor")
# m_f <-model_generator(processedDCE, "fixed")
#
# out1 <- runModel(m_1f, verbose = 1, C_code = "cool", ghq_steps = 100, shuffle = FALSE)
# out2 <- runModel(m_1f, verbose = 1, C_code = "orig", ghq_steps = 100, shuffle = TRUE)
#
# out1 <- runModel(m_f, verbose = 1, C_code = "cool", ghq_steps = 100, shuffle = FALSE)
# out2 <- runModel(m_f, verbose = 1, C_code = "orig", ghq_steps = 100, shuffle = TRUE)

#nhop > 1

library(DCM)
library(tidyverse)
library(fastGHQuad)

processedDCE <- setUp(DCEpriorities)

m_r <-model_generator(processedDCE, "random")

out1 <- runModel(m_r, verbose = 1, C_code = "C", ghq_steps = 100, shuffle = FALSE)

names1<-out1$results$parameters

k <- 10
size <- 1000
code_types <- c("gh", "C", "cool", "orig")
shuffles <- c(TRUE)

test <- matrix(NA, nrow = length(shuffles)*length(code_types)*k, ncol = 22)
i <- 0

for(a in 1:k){
  for(code in 1:length(code_types)){
    for(s in 1:length(shuffles)){
      i <- i + 1
      test[i, 1] <- code
      test[i, 2] <- s

      mod <- runModel(m_r, verbose = 0, C_code = code_types[code], ghq_steps = size, shuffle = shuffles[s])

      test[i,3:20] <- mod$results$estimate
      test[i,21] <- mod$LL
      test[i,22] <- mod$loglikf$iterations
    }
  }
  save(test, file = "./TESTING_DUMP/temp_workflow3_test_random_model_numerics_gh.RData")
  print(paste(a, "of", k, "replicates"))
}
#no shuffle -> always the same




load(file = "./TESTING_DUMP/temp_workflow3_test_random_model_numerics_gh.RData")
test %>% as.data.frame() %>% group_by(V1) %>% summarise(across(V3:V22, ~ abs(sd(.x)/mean(.x)))) -> test2
test2$V1[1:4] <- code_types
names(test2) <- c("code type", names1, "LL", "iterations")
test2

test %>% as.data.frame() %>% group_by(V1) %>% summarise(across(V3:V22, ~ mean(.x))) -> test2
test2$V1[1:4] <- code_types
names(test2) <- c("code type", names1, "LL", "iterations")

test2


#3568.789 29
# k<-10
# for(i in 1:k){
#   # m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
#   # test_old <- runModel(m1BWDCE_EMI_old, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
#
#   test_old <- runModel(m1BWDCE_EMI_old, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
#   results_list[[i]] <- test_old
#   save(results_list, file = "./TESTING_DUMP/temp_workflow3_test_results_100_numerics.RData")
# }


# k<-3
# for(i in 1:k){
#   # m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
#   # test_old <- runModel(m1BWDCE_EMI_old, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
#   #m1BWDCE_EMI_old <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
#
#   test_old <- runModel(m1BWDCE_EMI_old, verbose = 1, C_code = TRUE, ghq_steps = 100, shuffle = FALSE)
#
#   results_list[[i]] <- test_old
#   save(results_list, file = "./TESTING_DUMP/temp_workflow3_test_results_1000_noC_noShuf_numerics.RData")
# }

#
#   #old data (as close as possible), old emi
#   m1BWDCE_EMI_new <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
#   test_new <- runModel(m1BWDCE_EMI_new, verbose = 1, C_code = TRUE, ghq_steps = 10000)
#
#   m1BWDCE_EMI_old2 <- loadEMIWorkbook(processedBWDCE_old, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
#   test_old2 <- runModel(m1BWDCE_EMI_old2, verbose = 1, C_code = TRUE, ghq_steps = 10000)

#
# sapply(results_list, function (x) {x$old$LL})
#
#
# load("./TESTING_DUMP/temp_workflow3_test_results_1000_C_numerics.RData")
# res1000 <- results_list
#
# load("./TESTING_DUMP/temp_workflow3_test_results_100_numerics.RData")
# res100 <- results_list
#
#
# s100 <- array(NA, 63)
# u100 <- array(NA, 63)
#
# for(i in 1:63){
#
#   u100[i] <- mean(sapply(res100, function (x) {x$results$estimate[i]}))
#   s100[i] <- sd(sapply(res100, function (x) {x$results$estimate[i]}))
# }
#
# s1000 <- array(NA, 63)
# u1000 <- array(NA, 63)
#
# for(i in 1:63){
#
#   u1000[i] <- mean(sapply(res1000, function (x) {x$results$estimate[i]}))
#   s1000[i] <- sd(sapply(res1000, function (x) {x$results$estimate[i]}))
# }





# #new data (19), old emi
# m1BWDCE_EMI_mix <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx")
# test_mix <- runModel(m1BWDCE_EMI_mix, verbose = 1, C_code = FALSE, ghq_steps = 100)
#
# test_new$results
# test_old$results
