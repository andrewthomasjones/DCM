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

simulate_dataset <- function(template, model_type, chosen_values, Ron=FALSE){



  processed <- setUp(template)
  model <- model_generator(processed, model_type)
  test_sims <- simulation(chosen_values,  model,  processed)



  processed_sims <- setUp(test_sims)
  model_sims <- model_generator(processed_sims, model_type)
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

m <- 100
#-----------------------------
# define attributes and levels
#-----------------------------
desVarNames <- c("Safety", "Reliability", "Comfort")
desLevels <- c(4, 4, 4)
n <- 4       #number of choice sets
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
                             Reliability_DCE = design_ouput[, 2],
                             Comfort_DCE = design_ouput[, 3])
}
template_DCE <- bind_rows(list_df)


list_df <- list()



desVarNames <- c("Safety", "Reliability", "Comfort")
desLevels <- c(3, 3, 3)
n <- 4       #number of choice sets
desOpt <- 4  #num option per choice set
#generate full factorial
dat<-gen.factorial(desLevels, length(desLevels), varNames=desVarNames, center=TRUE)

dat[rowSums(dat)==0,]


combos <- matrix(c(c(0, 0, 1, -1, 1, -1) , c( -1, 1, 0, 0, -1, 1), c(1, -1, -1, 1, 0, 0)),ncol = 3)

combos2 <-rbind(combos[1:4, ], combos[3:6, ], combos[c(1:2, 5:6), ], combos[1:4, ])

size <- 4

for(i in 1:m){

  list_df[[i]] <- data.frame(ID=1000+i,
                             ChoiceSet = rep(1:4, each=size),
                             Choice = sample(c(rep(0,size-1), 1), size),
                             Safety_BW = combos2[, 1],
                             Reliability_BW = combos2[, 2],
                             Comfort_BW =  combos2[, 3])
}
template_BW <- bind_rows(list_df)

processed_template_BW <- setUp(template_BW)
processed_template_DCE <- setUp(template_DCE)

joined <- join_choicedatasets(processed_template_BW, processed_template_DCE)
model_fixed <- model_generator(joined, "one-factor")

chosen_values <- c(2, -1, -1, 1, -0.5,-0.5, rep(0.1, 6))
template <- joined$data_original
model_type <- "one-factor"

results <- simulate_dataset(template, model_type, chosen_values)

results$C$loglikf$estimate
round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)

############################################################################################

chosen_values <- c(2, -1, -1)
template <- template_DCE
model_type <- "fixed"

results <- simulate_dataset(template, model_type, chosen_values)

results$C$loglikf$estimate
round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)






#array(0, length(model_fixed$initial_values))



part1 <- rep(sample(1:3, size=9, replace = TRUE),2)
part2 <- sqrt(part1)
chosen_values <- c(part1, part2)
template <- processedBWDCE$data_original
model_type <- "random"

results <- simulate_dataset(template, model_type, chosen_values)

results$C$loglikf$estimate
results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################

# chosen_values <- sample(0:3, size=length(model_random$initial_values), replace = TRUE)
#
# template <- processedBWDCE$data_original
# model_type <- "one-factor"
#
# results <- simulate_dataset(template, model_type, chosen_values)
# results$C$loglikf$estimate
# results$R$loglikf$estimate
#
# round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
# round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################

processedBWDCE <- join_choicedatasets(processedBW_rem, processedDCE)
model_mtmm <- loadEMIWorkbook(processedBWDCE, "./TESTING_DUMP/EMI_mtmm.xlsx")

chosen_values <- c(rep(0,18), rep(0,36), sample(1:3, 9, replace = T))#model_mtmm$initial_values #sample(0:3, size=length(model_mtmm$initial_values), replace = TRUE)
template <- processedBWDCE$data_original
model_type <- "mtmm"

results <- simulate_dataset(template, model_type, chosen_values)
results$C$loglikf$estimate
results$R$loglikf$estimate

round(abs(chosen_values-results$C$loglikf$estimate)/results$C$results$standard_errors, 2)
round(abs(chosen_values-results$R$loglikf$estimate)/results$R$results$standard_errors, 2)

############################################################################################







