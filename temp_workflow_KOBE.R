library(tools)
library(readxl)
library(openxlsx)

source('./misc/old_R/KOBE/R/KOBEemi.R')
source('./misc/old_R/KOBE/R/KOBEcheckmodel.R')
source('./misc/old_R/KOBE/R/KOBEresample.R')
source('./misc/old_R/KOBE/R/KOBEll.R')
source('./misc/old_R/KOBE/R/KOBEllmax.R')
source('./misc/old_R/KOBE/R/KOBEparcount.R')
source("./misc/old_R/KOBE/R/KOBEfrequencydistribution.R")

dataname <- "./TESTING_DUMP/BWprioritiesDCEpriorities.RData"
EMI <- "./TESTING_DUMP/EMI_BWprioritiesDCEpriorities_MTMM-2.xlsx"

load(dataname) #processed
processed_data <- processed

ndraws <- 1000
big_list <- list()
reps <- 5


for (i in 1:reps){

  model <- KOBE_load_emi(EMI)

  draws <- KOBEdraws(model, ndraws)

  big_list[[i]] <- KOBEcheckmodel(model, draws, processed_data)
  save(big_list, file = "./TESTING_DUMP/temp_workflow_KOBE_test_results_1000.RData")
}


