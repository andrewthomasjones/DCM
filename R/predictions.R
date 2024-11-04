#' predict.dcm
#' @param results results object
#' @param data data to make predictions on
#' @returns predictions
#' @export
predictDCM <- function(results, data = NULL) {

  model <- results$model

  chosen_values <- results$results$estimate

  if (is.null(data)) {
    processed <-  results$model$data
  }else {
    processed <- data
  }

  concept  <-  processed$concept
  nmax_choiceset_size  <- processed$nmax_choiceset_size
  data <- processed$data

  ndecisionmakers  <-  processed$ndecisionmakers
  groups <- as.numeric(factor(data[, 1]))

  epsilonmatrix <- model$epsilon
  deltamatrix <- model$delta
  gammamatrix <- model$gamma
  deltamatrix <- model$delta
  betamatrix <- model$beta
  #phimatrix <- model$phi

  npp <- model$npp
  nhop <- model$nhop

  muepsilonparameters <- array(0, npp)
  sigmaepsilonparameters <- array(0, npp)

  mudeltaparameters <- array(0, nhop)
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

    if (epsilonmatrix[i, 1] == -1)
      muepsilonparameters[i] <- 1
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
      sigmaepsilonparameters[i] <- chosen_values[m]
    }

    if (epsilonmatrix[i, 2] == -1) {
      sigmaepsilonparameters[i] <- 1
    }
  }


  for (i in 1:nhop) {
    if (deltamatrix[i, 2] == 1) {
      m <- m + 1
      sigmadeltaparameters[i] <- chosen_values[m]
    }

    if (deltamatrix[i, 2] == -1) {
      sigmadeltaparameters[i] <- 1
    }
  }


  for (j in 1:nhop) {
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





  nlines  <-  dim(data)[1]



  delta <- mvtnorm::rmvnorm(ndecisionmakers,
                            mudeltaparameters,
                            diag(sigmadeltaparameters) * 0)

  epsilon <- mvtnorm::rmvnorm(ndecisionmakers,
                              muepsilonparameters,
                              diag(sigmaepsilonparameters) * 0)


  prob_matrix <- matrix(0, ncol = nmax_choiceset_size, nrow = nlines)
  choice_matrix <- matrix(0, ncol = nmax_choiceset_size, nrow = nlines)

  for (i in 1:nlines) {
    j <- groups[i]

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
    probs <- probs / sum(probs)
    prob_matrix[i, ] <- c(probs, rep(0, nmax_choiceset_size - length(probs)))
    choice_locator <- which(options  == data[i, 2])
    choice_matrix[i, choice_locator] <- 1
  }

  predictions <- prob_matrix
  choices <- data[, (1:nmax_choiceset_size) + 4]
  actual_choice <- data[, 2]

  ce_loss <- CELoss(choice_matrix, predictions)
  accuracy_val <- accuracy(choice_matrix, predictions)
  loss_result <- list(ce_loss = ce_loss,
                      accuracy = accuracy_val,
                      predictions = predictions,
                      true = choice_matrix,
                      choices = choices,
                      actual_choice = actual_choice)

  return(loss_result)
}


CELoss <- function(true, predictions) {

  loss <- 0

  for (i in seq_len(nrow(true))) {
    loss <- loss - as.numeric(true[i, ] %*% log(predictions[i, ]))
  }

  return(loss / nrow(true))
}

accuracy <- function(true, predictions) {
  correct <- 0

  for (i in seq_len(nrow(true))) {
    tmp <- array(0, ncol(true))
    tmp[which.max(predictions)] <- 1
    correct <- correct + as.numeric(true[i, ] %*% predictions[i, ])
  }
  acc <- correct / nrow(true)
  return(acc)
}


#' cvError
#' @param raw_dataset raw_dataset
#' @param k cv folds, 0 is LOOCV
#' @param type model type, doesn't do custom yet
#' @param seed random seed for cv folds
#' @param emi_filename filename is using EMI rather than standard model
#' @param integral_type  which method
#' @returns CV CE Loss
#' @export
cvError <- function(raw_dataset, k = 0, type = "fixed", seed = 1, emi_filename = NULL, integral_type = NULL) {

  dataset <- raw_dataset
  IDs <- unique(dataset$ID)

  if (k == 0) {
    folds <- as.list(IDs)
  }else {
    set.seed(seed)
    idx <- sample(1:k, size = length(IDs), replace = TRUE)
    folds <- list()
    for (i in 1:k){
      folds[[i]] <- IDs[idx == i]
    }
  }

  loss_vector <- array(0, length(folds))

  fold_list <- foreach::foreach(
    i = seq_len(length(folds)),
    .final = function(x) {
      setNames(x, paste0("fold_", seq_len(length(folds))))
    },
    .packages = "DCM"
  ) %dopar% {
    temp_list <- list()

    cli::cli_inform(paste0("fold_", i, " of ", length(folds)))

    train_data <- dataset[!dataset$ID %in% folds[[i]], ]
    train_processed <- setUp(train_data)

    test_data <- dataset[dataset$ID %in% folds[[i]], ]
    test_processed <- setUp(test_data)

    if (type != "EMI") {
      if (is.null(integral_type)) {
        integral_type <- "TMB"
      }
      model <- modelGenerator(train_processed, type)

    }else {
      if (is.null(integral_type)) {
        integral_type <- "GHQ"
      }
      model <- loadEMIWorkbook(train_processed, emi_filename)
    }
    res <- runModel(model, integral_type = integral_type)

    loss_fixed <- predictDCM(res, test_processed)
    return(list(loss = loss_fixed$ce_loss,
                acc = loss_fixed$accuracy,
                preds = loss_fixed$predictions,
                true = loss_fixed$true)
           )
  }

  return(fold_list)

}
