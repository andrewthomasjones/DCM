#' does sims
#' not used - C++ version is
#' @param chosen_values vector
#' @param model model to base simulations on
#' @param processed data to base simulations on
#' @returns scores
#' @export
scores <- function(chosen_values,  model,  processed) {

  concept  <-  processed$concept
  nmax_choiceset_size  <- processed$nmax_choiceset_size
  data  <-  processed$data
  ndecisionmakers  <-  processed$ndecisionmakers

  epsilonmatrix <- model$epsilon
  deltamatrix <- model$delta
  gammamatrix <- model$gamma
  deltamatrix <- model$delta
  betamatrix <- model$beta
  #phimatrix <- model$phi

  npp <- model$npp
  nhop <- model$nhop

  muepsilonparameters <- array(0, npp)
  mudeltaparameters <- array(0, nhop)
  sigmaepsilonparameters <- array(0, npp)
  sigmadeltaparameters <- array(0, nhop)

  gammaparameters <- gammamatrix * 0
  betaparameters <- betamatrix * 0
  #phiparameters <- phimatrix * 0

  m <- 0

  for (i in 1:npp) {

    if (epsilonmatrix[i, 1] == 1) {
      m <- m + 1
      muepsilonparameters[i] <- chosen_values[m]
    }

    if (epsilonmatrix[i, 1] == -1) muepsilonparameters[i] <- 1
  }

  for (i in 1:nhop) {

    if (deltamatrix[i, 1] == 1) {

      m <- m + 1
      mudeltaparameters[i] <- chosen_values[m]
    }

    if (deltamatrix[i, 1] == -1) {
      mudeltaparameters[i] <- 1
    }
  }


  for (i in 1:npp) {

    if (epsilonmatrix[i, 2] == 1) {
      m <- m + 1
      sigmaepsilonparameters[i] <- abs(chosen_values[m])
    }

    if (epsilonmatrix[i, 2] == -1) {
      sigmaepsilonparameters[i] <- 1
    }
  }


  for (i in 1:nhop) {

    if (deltamatrix[i, 2] == 1) {
      m <- m + 1
      sigmadeltaparameters[i] <- abs(chosen_values[m])
    }

    if (deltamatrix[i, 2] == -1) {
      sigmadeltaparameters[i] <- 1
    }
  }


  for (j in 1: nhop) {
    for (i in 1:npp) {
      if (gammamatrix[i, j] == 1) {
        m <- m + 1
        gammaparameters[i, j] <- chosen_values[m]
      }
      if (gammamatrix[i, j] == -1) {
        gammaparameters[i, j] <- 1
      }
    }
  }



  for (j in 1:nhop) {
    for (i in 1:nhop) {
      if (betamatrix[i, j] == 1) {
        m <- m + 1
        betaparameters[i, j] <- chosen_values[m]
      }
      if (betamatrix[i, j] == -1) {
        betaparameters[i, j] <- 1
      }
    }
  }


  gb <- diag(nhop) - betaparameters
  gb <- solve(gb)
  gb  <-  gammaparameters %*% gb


  delta <- matrix(0, nrow = 1, ncol = length(mudeltaparameters))
  epsilon <- matrix(0, nrow = 1, ncol= length(muepsilonparameters))

  gb  <-  gb %*% t(delta)
  gb <- gb + t(epsilon)


  conceptuse <- concept %*% model$code
  gb <- conceptuse %*% gb
  gb <- exp(gb)


  nlines  <-  dim(data)[1]

  scores <-  matrix(0, nrow = nlines , ncol = dim(concept)[2])

  for (i in 1:nlines) {

    options <- data[i, (1:nmax_choiceset_size) + 4]

    options <- options[options>0]
    probs <- gb[options, ]
    probs <- probs/sum(probs)
    choice <-  which(options == data[i, 2])
    d_i <- rep(0, length(probs))
    d_i[choice] <- 1

    scores[i, ] <- t(d_i - probs) %*% concept[options, ]
  }

  return(scores)
}











#' does sims
#' not used - C++ version is
#' @param chosen_values vector
#' @param model model to base simulations on
#' @param processed data to base simulations on
#' @returns loglik
#' @export
simulation <- function(chosen_values,  model,  processed) {

  concept  <-  processed$concept
  nmax_choiceset_size  <- processed$nmax_choiceset_size
  data  <-  processed$data
  ndecisionmakers  <-  processed$ndecisionmakers

  groups <- as.numeric(factor(model$data$data[,1]))

  epsilonmatrix <- model$epsilon
  deltamatrix <- model$delta
  gammamatrix <- model$gamma
  deltamatrix <- model$delta
  betamatrix <- model$beta
  #phimatrix <- model$phi

  npp <- model$npp
  nhop <- model$nhop

  muepsilonparameters <- array(0, npp)
  mudeltaparameters <- array(0, nhop)
  sigmaepsilonparameters <- array(0, npp)
  sigmadeltaparameters <- array(0, nhop)

  gammaparameters <- gammamatrix * 0
  betaparameters <- betamatrix * 0
  #phiparameters <- phimatrix * 0

  m <- 0

  for (i in 1:npp) {

    if (epsilonmatrix[i, 1] == 1) {
      m <- m + 1
      muepsilonparameters[i] <- chosen_values[m]
    }

    if (epsilonmatrix[i, 1] == -1) muepsilonparameters[i] <- 1
  }

  for (i in 1:nhop) {

    if (deltamatrix[i, 1] == 1) {

      m <- m + 1
      mudeltaparameters[i] <- chosen_values[m]
    }

    if (deltamatrix[i, 1] == -1) {
      mudeltaparameters[i] <- 1
    }
  }


  for (i in 1:npp) {

    if (epsilonmatrix[i, 2] == 1) {
      m <- m + 1
      sigmaepsilonparameters[i] <- abs(chosen_values[m])
    }

    if (epsilonmatrix[i, 2] == -1) {
      sigmaepsilonparameters[i] <- 1
    }
  }


  for (i in 1:nhop) {

    if (deltamatrix[i, 2] == 1) {
      m <- m + 1
      sigmadeltaparameters[i] <- abs(chosen_values[m])
    }

    if (deltamatrix[i, 2] == -1) {
      sigmadeltaparameters[i] <- 1
    }
  }


  for (j in 1: nhop) {
    for (i in 1:npp) {
      if (gammamatrix[i, j] == 1) {
        m <- m + 1
        gammaparameters[i, j] <- chosen_values[m]
      }
      if (gammamatrix[i, j] == -1) {
        gammaparameters[i, j] <- 1
      }
    }
  }



  for (j in 1:nhop) {
    for (i in 1:nhop) {
      if (betamatrix[i, j] == 1) {
        m <- m + 1
        betaparameters[i, j] <- chosen_values[m]
      }
      if (betamatrix[i, j] == -1) {
        betaparameters[i, j] <- 1
      }
    }
  }




  data2 <- data
  nlines  <-  dim(data)[1]

  delta <- rmvnorm(ndecisionmakers, mudeltaparameters, diag(sigmadeltaparameters))
  epsilon <- rmvnorm(ndecisionmakers, muepsilonparameters, diag(sigmaepsilonparameters))

  for (i in 1:nlines) {

    j = groups[i];

    gb <- diag(nhop) - betaparameters
    gb <- solve(gb)
    gb  <-  gammaparameters %*% gb
    gb  <-  gb %*% t(delta)[, j]
    gb <- gb + t(epsilon)[, j]

    conceptuse <- concept %*% model$code
    gb <- conceptuse %*% gb
    gb <- exp(gb)

    options <- data[i, (1:nmax_choiceset_size) + 4]
    probs <- gb[options, ]
    probs <- probs/sum(probs)
    sim_choice <- rmultinom(1, 1, probs)

    sim_choice_loc <- options[which.max(sim_choice)]
    data2[i, 2] <- sim_choice_loc

  }

  new_orig_data <- setNames(data.frame(matrix(ncol = 3+length(processed$attribute_names), nrow = 0)), c("ID", "ChoiceSet", "Choice", processed$attribute_names))


  concept2  <- processed$concept
  colnames(concept2) <- processed$attribute_names

  choice_set_counter <- 0
  ID_check <- data2[1, 1]

  for(j in 1:nrow(data2)){
    if(ID_check == data2[j, 1]){
      choice_set_counter <- choice_set_counter + 1
    }else{
      choice_set_counter <- 1
      ID_check <- data2[j, 1]
    }

    choice_locator <- which(data2[j, (1:nmax_choiceset_size) + 4]  ==     data2[j, 2])




    for(k in 1:nmax_choiceset_size){
      temp_list <- data.frame(matrix(c(data2[j, 1],choice_set_counter, 1*(choice_locator==k),  concept2[data2[j, k+4], ]), nrow=1))
      if(length(temp_list) > 3){
        names(temp_list) <- names(new_orig_data)
        new_orig_data <- bind_rows(new_orig_data, temp_list)
      }
    }

  }

  return(new_orig_data)
}

simulate_dataset <- function(processed, model_type, chosen_values,  easy_guess=FALSE){


  #processed <- setUp(template)
  model <- model_generator(processed, model_type)
  test_sims <- simulation(chosen_values,  model,  processed)



  processed_sims <- setUp(test_sims)
  model_sims <- model_generator(processed_sims, model_type)

  if(easy_guess){
    model_sims$initial_values <- chosen_values
  }

  return(model_sims)
}


estimate_model <- function(model_sims, type, precision, model_type){

  if(type=="ghq"){
    dev_mode <- "C"
    ghq_size <- precision

    tryCatch(
      expr = {
        results <- runModel(model_sims, dev_mode = dev_mode,  ghq_size = ghq_size, verbose = 0)
        return(results)
      },
      error = function(e){
        return(NA)
      }
    )



  }else if(type=="draws"){
    dev_mode <- "Cdraws"
    draws <- precision

    tryCatch(
      expr = {
        results <- runModel(model_sims, dev_mode = dev_mode,  draws = draws, verbose = 0)
        return(results)
      },
      error = function(e){
        return(NA)
      }
    )


  }else if(type == "TMB"){


    tryCatch(
      expr = {
        results <- run_model_TMB(model_sims$data, model_type)

        return(results)
      },
      error = function(e){
        return(NA)
      }
    )


  }


}


simulate_data <- function(m){
  #m <- 200
  #-----------------------------
  # define attributes and levels
  #-----------------------------
  desVarNames <- c("Safety", "Reliability")#, "Comfort", "Convenience")
  desLevels <- c(4, 4)#, 4, 4)
  n <- 2      #number of choice sets
  desOpt <- 4  #num option per choice set
  #generate full factorial
  dat<-gen.factorial(desLevels, length(desLevels), varNames=desVarNames, center=TRUE)

  destT <- optFederov(~., dat, nTrials = (n*(desOpt)), criterion="D")
  design_ouput <- destT$design*0.2



  list_df <- list()

  for(i in 1:m){

    list_df[[i]] <- data.frame(ID=1000+i,
                               ChoiceSet = rep(1:n, each=desOpt),
                               Choice = sample(c(rep(0,desOpt-1), 1), desOpt),
                               Safety_DCE = design_ouput[, 1],
                               Reliability_DCE = design_ouput[, 2]
    )
  }
  template_DCE <- bind_rows(list_df)


  l <- length(desLevels) + 1

  design_ouput2 <- crossdes::find.BIB(l, 4, 3, iter = 30)

  res <- matrix(0, nrow = 24, ncol = length(desLevels)+1)


  res <- matrix(c(
    c(1, -1, 0),
    c(-1, 1, 0),
    c(1, 0, -1),
    c(-1, 0, 1),
    c(0, 1, -1),
    c(0, -1, 1)
  ), ncol=3, byrow =TRUE)


  for(i in 1:m){

    list_df[[i]] <- data.frame(ID=1000+i,
                               ChoiceSet = rep(1:3, each=2),
                               Choice = sample(c(rep(0,2-1), 1), 2),
                               Safety_BW = res[, 1],
                               Reliability_BW = res[, 2],
                               Comfort_BW =  res[, 3]
    )

  }

  template_BW <- bind_rows(list_df)
  template_BW2 <- template_BW[, 1:5]


  processed_template_BW <- setUp(template_BW)
  processed_template_BW  <- remove_variables(processed_template_BW, "Comfort_BW")
  processed_template_DCE <- setUp(template_DCE)
  processed_template_BWDCE  <- join_choicedatasets(processed_template_BW , processed_template_DCE)

  processed <- list()

  processed[["DCE"]] <- processed_template_DCE
  processed[["BW"]] <- processed_template_BW
  processed[["BWDCE"]] <- processed_template_BWDCE

  return(processed)
}



library(DCM)
library(mvtnorm)
library(tidyverse)
library(AlgDesign)

source("TMB_test2.R")

big_list <- list()

n_sims <- 1000

m_list <- c(100, 250)  #c(20, 50, 100, 200)

models <- c("random", "one-factor", "mtmm", "fixed")
integral_types <- c("TMB", "draws", "ghq")

precision_levels <- list()

precision_levels[["draws"]] <- c(1000)#, 1000)
precision_levels[["ghq"]] <- c(3, 4)
precision_levels[["TMB"]] <- c(0)

chosen_values <- list()

chosen_values[["fixed"]][["DCE"]] <- c(1.2, 1.4)
chosen_values[["fixed"]][["BW"]] <- c(1.8, 0.7)
chosen_values[["fixed"]][["BWDCE"]] <- c(1.2, 1.4, 1.8, 0.7)

chosen_values[["random"]][["DCE"]] <- c(0.7, 2.0, 2.8, 1.8)
chosen_values[["random"]][["BW"]] <- c(1.3, 0.5, 3.0, 1.8)
chosen_values[["random"]][["BWDCE"]] <- c(0.7, 2.0, 1.3, 0.5, 2.8, 1.8, 3.0, 1.8)

chosen_values[["one-factor"]][["DCE"]] <- c(2.2, 1.6, 1.9, 0.2)
chosen_values[["one-factor"]][["BW"]] <- c(2.4, 0.7, 1.5, 0.0)
chosen_values[["one-factor"]][["BWDCE"]] <- c(2.2, 1.6, 2.4, 0.7, 1.9, 0.2, 1.5, 0.0)

chosen_values[["mtmm"]][["DCE"]] <- NA
chosen_values[["mtmm"]][["BW"]] <- NA
chosen_values[["mtmm"]][["BWDCE"]] <- c(3.0,  1.9,  3.0,  0.5, 2.5,  1.5,  1.5, -1.5,  2.7,  2.2,  2.1,  0.6,  0.9, -0.5)

data_sets <- c("DCE", "BW", "BWDCE")
big_list <- list()

file <- "./TESTING_DUMP/simulation_saved_results_TMB_fixed2.Rdata"

for(m in m_list){

  m_size <- paste0("m_", m)

  processed <- simulate_data(m)

  for(model_type in models){

    for(data_type in data_sets){

      big_list[[m_size]][[model_type]][[data_type]][["specs"]] <- list(values = chosen_values[[model_type]][[data_type]], n_sims = n_sims, m=m, easy_guess=TRUE, template = processed[[data_type]])

      if(!is.na(big_list[[m_size]][[model_type]][[data_type]][["specs"]][1])){
        for(i in 1:n_sims){
          n_name <- paste0("n_", i)
          message(paste(m_size, model_type, data_type, i))
          big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][["sim"]] <- simulate_dataset(processed[[data_type]], model_type, chosen_values[[model_type]][[data_type]], easy_guess=TRUE)

          for(g in integral_types){
            for(p in precision_levels[[g]]){
              p_name <- paste0("p_", p)
              message(paste(".   sim:", i, g, p_name))
              #p <- as.numeric(str_extract(p_name, "([0-9].*)"))
              big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][[g]][[p_name]] <- estimate_model(big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][["sim"]], g, p, model_type)
            }
            save(big_list, file=file)
            gc()
          }
        }
      }

    }
  }
}


# bias - is estimator estimating true value
# variance - variance of estimator / MSE of estimator
# sample standard error of estimates
# z scores of (true - est)/se -> is it standard normal
# do 95% CIs have appropriate coverage probs
# does bias and variance differ between Draws and GHQ
# does bias and variance differ between GHQ levels
# does bias and variance differ between Draws levels

# overall and divided by data type / model type / 'variable class'

# what is appropriate GHQ level (judgement call)
# is GHQ estimator unbiased
# are reported standard errors correct
# are reported CIs adequate (are true CIs normal)
# just quite how bad was the draws thing


# sapply(big_list$m_20$fixed$DCE$results, "[[", "draws$p_100$results$estimate")- big_list$m_20$fixed$DCE$specs$values
#
#
# big_list$m_20$fixed$DCE$results$n_1$draws$p_100$results$estimate - big_list$m_20$fixed$DCE$specs$values
# big_list$m_20$fixed$DCE$results$n_1$draws$p_1000$results$estimate - big_list$m_20$fixed$DCE$specs$values
# big_list$m_20$fixed$DCE$results$n_1$ghq$p_3$results$estimate - big_list$m_20$fixed$DCE$specs$values
# big_list$m_20$fixed$DCE$results$n_1$ghq$p_4$results$estimate - big_list$m_20$fixed$DCE$specs$values
# big_list$m_20$fixed$DCE$results$n_1$ghq$p_5$results$estimate -  big_list$m_20$fixed$DCE$specs$values
#

# file <- "./TESTING_DUMP/simulation_saved_results2.Rdata"
# load(file=paste0(file))
# #
# model <- "random"
# size <- "m_10"
# data <- "BWDCE"
# random_type <- "ghq"
# precision_level <- "p_3"
#
# bias <- rep(0, length(big_list[[size]][[model]][[data]]$specs$values))
#
# for(i in names(big_list[[size]][[model]][[data]]$results)){
#   bias <- bias + (big_list[[size]][[model]][[data]]$results[[i]][[random_type]][[precision_level]]$results$estimate - big_list[[size]][[model]][[data]]$specs$values)/length(names(big_list[[size]][[model]][[data]]$results))
# }
#
# var_names <- big_list[[size]][[model]][[data]]$results$n_1$ghq$p_3$results$parameters
# bias
# #



#
#
#
#
# for (m in m_list){
#
#   m_size <- paste0("m_", m)
#
#   processed <- simulate_data(m)
#
#   for(model_type in models){
#
#     for(data_type in names(processed)){
#
#       big_list[[m_size]][[model_type]][[data_type]][["specs"]] <- list(values = chosen_values[[model_type]][[data_type]], n_sims = n_sims, m=m, easy_guess=TRUE, template = processed[[data_type]])
#
#       if(!is.na(big_list[[m_size]][[model_type]][[data_type]][["specs"]][1])){
#         for(i in 1:n_sims){
#           n_name <- paste0("n_", i)
#           message(paste(m_size, model_type, data_type))
#           big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][["sim"]] <- simulate_dataset(processed[[data_type]], model_type, chosen_values[[model_type]][[data_type]], easy_guess=TRUE)
#
#           for(g in integral_types){
#             for(p in precision_levels[[g]]){
#               p_name <- paste0("p_", p)
#               message(paste(".   sim:", i, g, p_name))
#               big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][[g]][[p_name]] <- estimate_model(big_list[[m_size]][[model_type]][[data_type]][["results"]][[n_name]][["sim"]], g, p)
#             }
#           }
#         }
#       }
#       save(big_list, file=paste0("./TESTING_DUMP/simulation_", format(Sys.time(), "%Y-%m-%d_%H%m"), "_saved_results.Rdata"))
#     }
#   }
# }




