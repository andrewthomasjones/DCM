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
      new_orig_data <- new_orig_data %>% add_row(ID = data2[j, 1], ChoiceSet = choice_set_counter, Choice = 1*(choice_locator==k),   processed$data_original[data2[j, k+4], 4:(3+length(processed$attribute_names))])
    }

  }

  return(new_orig_data)
}

simulate_dataset <- function(template, model_type, chosen_values){



  processed <- setUp(template)
  model <- model_generator(processed, model_type)
  test_sims <- simulation(chosen_values,  model,  processed)



  processed_sims <- setUp(test_sims)
  model_sims <- model_generator(processed_sims, model_type)
  results_sims_C <- runModel(model_sims,  dev_mode = "C",  ghq_size = 3, verbose = 0)
  results_sims_R <- runModel(model_sims,  dev_mode = "C",  ghq_size = 3, verbose = 0)
  return(list(C = results_sims_C, R = results_sims_R))
}


processedDCE <- setUp(DCEpriorities)
model_fixed <- model_generator(processedDCE, "fixed")
model_random <- model_generator(processedDCE, "random")
model_1f <- model_generator(processedDCE, "one-factor")



#array(0, length(model_fixed$initial_values))

chosen_values <- sample(0:3, size=length(model_fixed$initial_values), replace = TRUE)
template <- DCEpriorities
model_type <- "fixed"

results <- simulate_dataset(template, model_type, chosen_values)

results$C$loglikf$estimate
results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)
############################################################################################

chosen_values <- sample(0:3, size=length(model_1f$initial_values), replace = TRUE)
template <- DCEpriorities
model_type <- "one-factor"

results <- simulate_dataset(template, model_type, chosen_values)

results$C$loglikf$estimate
results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################

chosen_values <- sample(0:3, size=length(model_random$initial_values), replace = TRUE)
template <- DCEpriorities
model_type <- "random"

results <- simulate_dataset(template, model_type, chosen_values)
results$C$loglikf$estimate
results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################


processedDCE <- setUp(DCEpriorities[DCEpriorities$ID < 1030, ])
processedBW <- setUp(BWpriorities[BWpriorities$ID < 1030, ])
processedBW_rem <- remove_variables(processedBW, "Accessibility_BW")

processedBWDCE <- join_choicedatasets(processedBW_rem, processedDCE)
model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_mtmm.xlsx")

chosen_values <- sample(0:3, size=length(model_mtmm$initial_values), replace = TRUE)
template <- processedBWDCE$data_original
model_type <- "mtmm"

results <- simulate_dataset(template, model_type, chosen_values)
results$C$loglikf$estimate
#results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
#round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################







