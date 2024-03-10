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


  delta <- rmvnorm(1, mudeltaparameters, diag(sigmadeltaparameters))
  epsilon <- rmvnorm(1, muepsilonparameters, diag(sigmaepsilonparameters))

  gb  <-  gb %*% t(delta)
  gb <- gb + t(epsilon)


  conceptuse <- concept %*% model$code
  gb <- conceptuse %*% gb
  gb <- exp(gb)


  data2 <- data
  nlines  <-  dim(data)[1]

  for (i in 1:nlines) {

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

simulate_dataset <- function(processed, model_type, chosen_values, Ron=FALSE, easy_guess=FALSE){



  #processed <- setUp(template)
  model <- model_generator(processed, model_type)
  test_sims <- simulation(chosen_values,  model,  processed)



  processed_sims <- setUp(test_sims)
  model_sims <- model_generator(processed_sims, model_type)

  if(easy_guess){
    model_sims$initial_values <- chosen_values
  }

  results_sims_C <- runModel(model_sims,  dev_mode = "C",  ghq_size = 3, verbose = 0)

  if(Ron){
    results_sims_R <- runModel(model_sims,  dev_mode = "C",  ghq_size = 3, verbose = 0)
  }else{
    results_sims_R <- NA
  }
  return(list(C = results_sims_C, R = results_sims_R))
}

library(DCM)
library(mvtnorm)
library(tidyverse)
library(AlgDesign)

big_list <- list()
n_sims <- 1000
m_list <- c(100, 200, 500, 1000)

for (m in m_list){

  m_size <- paste0("m_", m)
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




  ############################################################################################

  processed1 <- processed_template_DCE
  chosen_values1 <- c(2, 1)

  processed2 <- processed_template_BW
  chosen_values2 <- c(2, 1)

  processed3 <- processed_template_BWDCE
  chosen_values3 <- c(2, 1, 2, 1)

  model_type <- "fixed"

  big_list[[m_size]][[model_type]][["DCE"]][["specs"]] <- list(values = chosen_values1, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed1)
  big_list[[m_size]][[model_type]][["BW"]][["specs"]] <- list(values = chosen_values2, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed2)
  big_list[[m_size]][[model_type]][["BWDCE"]][["specs"]] <- list(values = chosen_values3, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed3)

  for(i in 1:n_sims){

    big_list[[m_size]][[model_type]][["DCE"]][["results"]][[i]] <- simulate_dataset(processed1, model_type, chosen_values1, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BW"]][["results"]][[i]] <- simulate_dataset(processed2, model_type, chosen_values2, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BWDCE"]][["results"]][[i]] <- simulate_dataset(processed3, model_type, chosen_values3, Ron=FALSE, easy_guess=TRUE)
    save(big_list, file="./TESTING_DUMP/simulations.Rdata")
  }

  # ks.test(list1, "pnorm", mean = 0, sd = 1) # DCE C
  # ks.test(list2, "pnorm", mean = 0, sd = 1) # BW C
  # ks.test(list3, "pnorm", mean = 0, sd = 1) # BWDCE C


  # ############################################################################################


  model_type <- "random"

  chosen_values1 <- c(2, 1, 0.2, 0.1)
  chosen_values2 <- c(2, 1, 0.2, 0.1)
  chosen_values3 <- c(2, 1, 2, 1, 0.2, 0.1, 0.2, 0.1)

  big_list[[m_size]][[model_type]][["DCE"]][["specs"]] <- list(values = chosen_values1, n_sims = n_sims, m=m, easy_guess=TRUE,template = processed1)
  big_list[[m_size]][[model_type]][["BW"]][["specs"]] <- list(values = chosen_values2, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed2)
  big_list[[m_size]][[model_type]][["BWDCE"]][["specs"]] <- list(values = chosen_values3, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed3)

  for(i in 1:n_sims){

    big_list[[m_size]][[model_type]][["DCE"]][["results"]][[i]] <- simulate_dataset(processed1, model_type, chosen_values1, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BW"]][["results"]][[i]] <- simulate_dataset(processed2, model_type, chosen_values2, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BWDCE"]][["results"]][[i]] <- simulate_dataset(processed3, model_type, chosen_values3, Ron=FALSE, easy_guess=TRUE)
    save(big_list, file="./TESTING_DUMP/simulations.Rdata")
  }

  # ks.test(list1, "pnorm", mean = 0, sd = 1) # DCE C
  # ks.test(list2, "pnorm", mean = 0, sd = 1) # BW C
  # ks.test(list3, "pnorm", mean = 0, sd = 1) # BWDCE C


  # ############################################################################################


  model_type <- "one-factor"

  chosen_values1 <- c(2, 1, 0.5, 0.5)
  chosen_values2 <- c(2, 1, 0.5, 0.5)
  chosen_values3 <- c(2, 1, 2, 1, 0.5, 0.5, 0.5, 0.5)

  big_list[[m_size]][[model_type]][["DCE"]][["specs"]] <- list(values = chosen_values1, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed1)
  big_list[[m_size]][[model_type]][["BW"]][["specs"]] <- list(values = chosen_values2, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed2)
  big_list[[m_size]][[model_type]][["BWDCE"]][["specs"]] <- list(values = chosen_values3, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed3)

  for(i in 1:n_sims){

    big_list[[m_size]][[model_type]][["DCE"]][["results"]][[i]] <- simulate_dataset(processed1, model_type, chosen_values1, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BW"]][["results"]][[i]] <- simulate_dataset(processed2, model_type, chosen_values2, Ron=FALSE, easy_guess=TRUE)
    big_list[[m_size]][[model_type]][["BWDCE"]][["results"]][[i]] <- simulate_dataset(processed3, model_type, chosen_values3, Ron=FALSE, easy_guess=TRUE)
    save(big_list, file="./TESTING_DUMP/simulations.Rdata")
  }


  # ############################################################################################

  #createEMIWorkbook(processed3,  "mtmm",  working_folder = "./TESTING_DUMP")
  model_mtmm <- loadEMIWorkbook(processed3, "./TESTING_DUMP/EMI_mtmm.xlsx")
  model_type <- "mtmm"


  #runModel(model_mtmm,  dev_mode = "C",  ghq_size = 3, verbose = 2)


  chosen_values3 <- c(
    c(2, 1, 2, 1),
    c(2, 3, -.2, 0.4),
    c(3, 1, 0.6, 0.1),
    c(-0.3, -1.2)
  )


  big_list[[m_size]][[model_type]][["DCE"]][["specs"]] <- NA
  big_list[[m_size]][[model_type]][["BW"]][["specs"]] <- NA
  big_list[[m_size]][[model_type]][["BWDCE"]][["specs"]] <- list(values = chosen_values3, n_sims = n_sims, m=m, easy_guess=TRUE, template = processed3)

  for(i in 1:n_sims){

      big_list[[m_size]][[model_type]][["BWDCE"]][["results"]][[i]] <- simulate_dataset(processed3, model_type, chosen_values3, Ron=FALSE, easy_guess=TRUE)
      save(big_list, file="./TESTING_DUMP/simulations.Rdata")
  }



  #ks.test(list1, "pnorm", mean = 0, sd = 1) # DCE C
  #ks.test(list2, "pnorm", mean = 0, sd = 1) # BW C
  #ks.test(list3_mtmm, "pnorm", mean = 0, sd = 1) # BWDCE C

  save(big_list, file="./TESTING_DUMP/simulations.Rdata")

}

save(big_list, file="./TESTING_DUMP/simulations.Rdata")







