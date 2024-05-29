library(DCM)
library(tidyverse)
library(doParallel)
registerDoParallel(cores=6)

chosen_values <- list()

# colvars <- 2
# chosen_values[["fixed"]][["DCE"]] <- c(1.2, 1.4)
# chosen_values[["fixed"]][["BW"]] <- c(1.8, 0.7)
# chosen_values[["fixed"]][["BWDCE"]] <- c(1.2, 1.4, 1.8, 0.7)
#
# chosen_values[["random"]][["DCE"]] <- c(0.7, 2.0, 2.8, 1.8)
# chosen_values[["random"]][["BW"]] <- c(1.3, 0.5, 3.0, 1.8)
# chosen_values[["random"]][["BWDCE"]] <- c(0.7, 2.0, 1.3, 0.5, 2.8, 1.8, 3.0, 1.8)
#
# chosen_values[["one-factor"]][["DCE"]] <- c(2.2, 1.6, 1.9, 0.2)
# chosen_values[["one-factor"]][["BW"]] <- c(2.4, 0.7, 1.5, 0.0)
# chosen_values[["one-factor"]][["BWDCE"]] <- c(2.2, 1.6, 2.4, 0.7, 1.9, 0.2, 1.5, 0.0)
#
# chosen_values[["mtmm"]][["DCE"]] <- NA
# chosen_values[["mtmm"]][["BW"]] <- NA
# chosen_values[["mtmm"]][["BWDCE"]] <- c(3.0,  1.9,  3.0,  0.5, 2.5,  1.5,  1.5, -1.5,  2.7,  2.2,  2.1,  0.6,  0.9, -0.5)

data_sets <- c("DCE", "BW", "BWDCE")
integral_types <- c("TMB", "draws")#, "ghq")

precision_levels <- list()
precision_levels[["draws"]] <- c(100, 1000, 5000, 10000, 50000)
precision_levels[["TMB"]] <- c(0)

n_sims <-  200
m_list <- c(200) #, 100, 250)
models <- c("one-factor",  "random", "fixed", "mtmm")
#precision_levels[["ghq"]] <- c(3, 4)

chosen_values <- list()

colvars <- 3



  filename <- paste0("./TESTING_DUMP/par_test_20240529_draws.Rdata")

  processedDCE <- setUp(DCEpriorities)
  processedDCE <- select_variables(processedDCE , processedDCE$attribute_names[seq_len(colvars)])

  processedBW <- setUp(BWpriorities)
  processedBW <- select_variables(processedBW , processedBW$attribute_names[seq_len(colvars)])

  processedBWDCE <- join_choicedatasets(processedBW, processedDCE)

  ###################################
  model_1f <- model_generator(processedDCE, "one-factor")
  model_fixed <- model_generator(processedDCE, "fixed")
  model_random <- model_generator(processedDCE, "random")

  res_1f <- runModel(model_1f)
  res_fixed <- runModel(model_fixed)
  res_random <- runModel(model_random)

  chosen_values[["one-factor"]][["DCE"]] <- res_1f$results$estimate
  chosen_values[["fixed"]][["DCE"]] <- res_fixed$results$estimate
  chosen_values[["random"]][["DCE"]] <- res_random$results$estimate

  ###################################
  model_1f <- model_generator(processedBW, "one-factor")
  model_fixed <- model_generator(processedBW, "fixed")
  model_random <- model_generator(processedBW, "random")

  res_1f <- runModel(model_1f)
  res_fixed <- runModel(model_fixed)
  res_random <- runModel(model_random)

  chosen_values[["one-factor"]][["BW"]] <- res_1f$results$estimate
  chosen_values[["fixed"]][["BW"]] <- res_fixed$results$estimate
  chosen_values[["random"]][["BW"]] <- res_random$results$estimate

  ###################################
  model_1f <- model_generator(processedBWDCE, "one-factor")
  model_fixed <- model_generator(processedBWDCE, "fixed")
  model_random <- model_generator(processedBWDCE, "random")
  model_mtmm <- model_generator(processedBWDCE, "mtmm")

  res_1f <- runModel(model_1f)
  res_fixed <- runModel(model_fixed)
  res_random <- runModel(model_random)
  res_mtmm <- runModel(model_mtmm)

  chosen_values[["one-factor"]][["BWDCE"]] <- res_1f$results$estimate
  chosen_values[["fixed"]][["BWDCE"]] <- res_fixed$results$estimate
  chosen_values[["random"]][["BWDCE"]] <- res_random$results$estimate
  chosen_values[["mtmm"]][["BWDCE"]] <- res_mtmm$results$estimate

  chosen_values[["mtmm"]][["DCE"]] <- NA
  chosen_values[["mtmm"]][["BW"]] <- NA

  big_list <- run_sims(data_sets, chosen_values, precision_levels, integral_types, models, m_list, n_sims, colvars, filename)

#
#
#
#
#   filename <- paste0("./TESTING_DUMP/par_test_20240529_draws.Rdata")
#
#   gc()
#   load(file=filename)
#   sim_results <- process_sims(big_list, params$data_sets, params$chosen_values, params$precision_levels, params$integral_types, params$models, params$m_list, params$n_sims,  0.95, params$colvars)
#   sim_results$n_cols <- colvars
#
#
#
#
#
# sim_results %>% filter(integral_type != "ghq") %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, y=coverage_probability2)) +
#   geom_point(size=2, position=position_dodge(0.5)) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_hline(yintercept=0.95, linetype="dashed") + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x")
#
# sim_results %>% filter(str_detect(name, "delta_sig")) %>% filter(integral_type != "ghq") %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, y=mu2)) +
#   geom_point(size=2, position=position_dodge(0.5)) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") + geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5)
#
#
#
# sim_results %>% filter(n_cols == 4)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, shape=factor(n_cols) , y=mu)) +
#   geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5) +
#   geom_point(size=2,  position=position_dodge(0.5)) + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylim(c(-1.8, 5))
#
#
# sim_results %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, y=coverage_probability)) +
#   geom_point(size=2, position=position_dodge(0.5)) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_hline(yintercept=0.95, linetype="dashed") + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x")
#
# sim_results %>% filter(integral_type != "ghq") %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, y=coverage_probability2)) +
#   geom_point(size=2, position=position_dodge(0.5)) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_hline(yintercept=0.95, linetype="dashed") + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x")
#
#
# sim_results %>% filter(integral_type != "ghq") %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, shape=factor(n_cols) , y=mu2)) +
#   geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5) +
#   geom_point(size=2,  position=position_dodge(0.5)) + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#
# sim_results %>% filter(str_detect(name, "delta_sig")) %>% filter(integral_type != "ghq") %>% filter(n_cols == 3)  %>%  filter(start=="eg_TRUE") %>%
#   ggplot(aes(x=name, colour=integral_type, shape=factor(n_cols) , y=mu)) +
#   geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5) +
#   geom_point(size=2,  position=position_dodge(0.5)) + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#
# sim_results %>% filter(data_type == "BWDCE") %>% filter(integral_type != "ghq") %>% filter(n_cols == 4)  %>%  filter(start=="eg_TRUE") %>%
#   ggplot(aes(x=name, colour=integral_type, shape=factor(n_cols) , y=mu)) +
#   geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5) +
#   geom_point(size=2,  position=position_dodge(0.5)) + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylim(c(-1.8, 6))
#
#
# sim_results %>% filter(data_type == "BWDCE") %>% filter(integral_type != "ghq") %>% filter(n_cols == 4)  %>%  filter(start=="eg_TRUE") %>%
#   ggplot(aes(x=name, colour=integral_type, shape=factor(n_cols) , y=mu2)) +
#   geom_point(aes(y = true), size=2,  position=position_dodge(0.5), colour="black", alpha=0.5) +
#   geom_point(size=2,  position=position_dodge(0.5)) + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylim(c(-1.8, 6))
#
# sim_results %>% filter(data_type == "BWDCE") %>% filter(integral_type != "ghq") %>% filter(n_cols == 4)  %>%  filter(start=="eg_TRUE") %>% ggplot(aes(x=name, colour=integral_type, y=coverage_probability2)) +
#   geom_point(size=2, position=position_dodge(0.5)) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   geom_hline(yintercept=0.95, linetype="dashed") + facet_grid(cols = vars(model_type), rows=vars(data_type), scales="free_x")
#
#
#
# #
# # #
# # #
# # #   colvars <- 2
# # #   filename <- paste0("./TESTING_DUMP/par_test_20240422_BW_", colvars, ".Rdata")
# # #   load(file=filename)
# # #   print(colvars)
# # #   res[[1]] <- process_sims(big_list, params$data_sets, params$chosen_values, params$precision_levels, params$integral_types, params$models, params$m_list, params$n_sims)
# # #   res[[1]]$cols <- 2
# # #
# # #   colvars <- 3
# # #   filename <- paste0("./TESTING_DUMP/par_test_20240422_BW_", colvars, ".Rdata")
# # #   load(file=filename)
# # #   print(colvars)
# # #   res[[2]] <- process_sims(big_list, params$data_sets, params$chosen_values, params$precision_levels, params$integral_types, params$models, params$m_list, params$n_sims)
# # #   res[[2]]$cols <- 3
# # #
# # #   # colvars <- 4
# # #   # filename <- paste0("./TESTING_DUMP/par_test_20240422_BW_", colvars, ".Rdata")
# # #   # load(file=filename)
# # #   # print(colvars)
# # #   # res[[3]] <- process_sims(big_list, params$data_sets, params$chosen_values[c(1,3)], params$precision_levels, params$integral_types, params$models[1:2], params$m_list, params$n_sims)
# # #
# # #   all_BW <- bind_rows(res)
# # #
# # #   all_BW %>% filter(cols==3) %>% filter(model_type == "random") %>% ggplot(aes(x=name, colour=integral_type, shape = start , y=coverage_probability)) +
# # #     geom_point(size=2) + facet_grid(cols = vars(data_type), scales="free_x") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_hline(yintercept=0.95, linetype="dashed")
# # #
# #
# #
# #
# #
# #
# # #
# # # gc()
# # # load(file=filename)
# # # sim_results <- process_sims(big_list, params$data_sets, params$chosen_values, params$precision_levels, params$integral_types, params$models, params$m_list, params$n_sims,  0.95, params$colvars)
# #
# # # sim_results
# # # sim_results %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# # # #
# # # #
# # #
# # # filename <- "./TESTING_DUMP/simulation_saved_results0.Rdata"
# # #
# # # n_sims <-  1
# # #
# # # m_list <- c(100) #, 100, 250)
# # #
# # # models <- c("one-factor",  "random", "fixed")
# # #
# # # integral_types <- c("TMB", "draws", "ghq")
# # #
# # # precision_levels <- list()
# # #
# # # precision_levels[["draws"]] <- c(1000)
# # # precision_levels[["ghq"]] <- c(3)
# # # precision_levels[["TMB"]] <- c(0)
# # #
# # # chosen_values <- list()
# # #
# # # chosen_values[["fixed"]][["DCE"]] <- c(1.2, 1.4, 0.5)
# # # chosen_values[["fixed"]][["BW"]] <- c(1.8, 0.7, 0.6)
# # #
# # # chosen_values[["random"]][["DCE"]] <- c(0.7, 2.0, 1.0, 2.8, 1.8, 1.5)
# # # chosen_values[["random"]][["BW"]] <- c(1.3, 0.5, 1.0, 3.0, 1.8, 1.5)
# # #
# # # chosen_values[["one-factor"]][["DCE"]] <- c(2.2, 1.6, 1.0, 1.9, 0.2, 1.0)
# # chosen_values[["one-factor"]][["BW"]] <- c(2.4, 0.7, 1.0, 1.5, 0.0, 1.0)
# #
# # data_sets <- c("DCE", "BW")
# #
# # p <- 3
# #
# # big_list <- run_sims(data_sets, chosen_values, precision_levels, integral_types, models, m_list, n_sims, p, filename)
# # rm(list = ls())
# # filename <- "./TESTING_DUMP/simulation_saved_results0.Rdata"
# # load(file=filename)
# # sim_results <- process_sims(big_list, params$data_sets, params$chosen_values, params$precision_levels, params$integral_types, params$models, params$m_list, params$n_sims)
# #
# #
# # #
# # #
# # # library(tidyverse)
# # #
# # # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_50_100_mtmm.Rdata")
# # # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_50_100_f_1f_r.Rdata")
# # # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_250_mtmm.Rdata")
# # # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_saved_results_250_f_1f_r.Rdata")
# # #
# # #
# # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_processed_results_250_mtmm.Rdata")
# # # results_table1 <- results_table
# # # load(file="/Users/uqajon14/Documents/Projects/DCM/TESTING_DUMP/simulation_processed_results_250_f_1f_r.Rdata")
# # # results_table2 <- results_table
# # #
# # #
# # # results <- rbind(results_table1, results_table2)
# # #
# # # results %>% filter(model_type == "fixed") -> results_fixed
# # #
# # # results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# # #
# # # results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# # #
# # # results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# # #
# # # results_fixed %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# # #
# # # results_fixed %>% summarise(good = 100*mean(good_estimate))
# # #
# # #
# # #
# #
# # results %>% filter(model_type == "one-factor") %>% filter(precision != 3)-> results_1f
# #
# # results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias_pc))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_1f %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_1f %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))
# #
# #
# # results_1f %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # results_table %>% filter(model_type == "random")  -> results_random
# #
# # results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_random %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_random %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))
# #
# #
# # results_random %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))
# #
# #
# #
# #
# # results %>% filter(model_type == "mtmm")  -> results_mtmm
# #
# # results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=coverage_probability)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=time)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=abs(bias_pc))) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_mtmm %>% ggplot(aes(x=name, colour=data_type, shape =start , y=mse)) + geom_point(size=2) + facet_wrap(~integral_type) + theme_bw()
# #
# # results_mtmm %>% group_by(integral_type, data_type) %>% summarise(good = 100*mean(good_estimate))
# #
# #
# # results_mtmm %>% group_by(integral_type, data_type, precision) %>% summarise(good = mean(coverage_probability))
# #
# #
# #
# #
