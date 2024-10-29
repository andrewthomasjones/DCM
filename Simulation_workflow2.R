library(DCM)
library(tidyverse)
library(doParallel)
registerDoParallel(cores=4)

chosen_values <- list()
colvars <- 2

chosen_values[["fixed"]][["DCE"]] <- c(1.2, 1.4)
chosen_values[["fixed"]][["BW"]] <- c(1.8, 0.7)
chosen_values[["fixed"]][["BWDCE"]] <- c(1.2, 1.4, 1.8, 0.7)

chosen_values[["random"]][["DCE"]] <- c(0.7, 2.0, 2.8, 1.8)
chosen_values[["random"]][["BW"]] <- c(1.3, 0.5, 3.0, 1.8)
chosen_values[["random"]][["BWDCE"]] <- c(0.7, 2.0, 1.3, 0.5, 2.8, 1.8, 3.0, 1.8)

chosen_values[["one-factor"]][["DCE"]] <- c(2.2, 1.6, 1.9, 0.6)
chosen_values[["one-factor"]][["BW"]] <- c(2.4, 0.7, 1.5, 0.8)
chosen_values[["one-factor"]][["BWDCE"]] <- c(2.2, 1.6, 2.4, 0.7, 1.9, 0.6, 1.5, 0.8)

chosen_values[["mtmm"]][["DCE"]] <- NA
chosen_values[["mtmm"]][["BW"]] <- NA
chosen_values[["mtmm"]][["BWDCE"]] <- c(3.0,  1.9,  3.0,  0.5, 2.5,  1.5,  1.5, 1.5,  2.7,  2.2,  2.1,  0.6,  0.9, 0.5)

data_sets <- c("DCE", "BW", "BWDCE")
integral_types <- c("TMB", "draws")#, "ghq")

precision_levels <- list()
precision_levels[["draws"]] <- c(500, 1000, 2000, 4000)
precision_levels[["TMB"]] <- c(0)
#precision_levels[["ghq"]] <- c(3)

n_sims <-  300
m_list <- c(250) #, 100, 250)
models <- c("one-factor",  "random", "mtmm") #"fixed",


filename <- "./TESTING_DUMP/par_test_20241028_NdrawsTest.Rdata"

big_list <- run_sims(data_sets, chosen_values, precision_levels, integral_types, models, m_list, n_sims, colvars, filename)

