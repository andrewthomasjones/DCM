#' calculates optimum model fit
#' not used - C++ version is
#' @param model model list
#' @param processed data list
#' @param ghq_matrix1 mat
#' @param nlm_params list of params
#' @returns max loglik
#' @export
llMax  <- function(model,  processed, ghq_matrix1, nlm_params) {

  loglik  <-  nlm(f = llCalc,
                  p = model$initial_values,
                  model = model,
                  processed = processed,
                  ghq_matrix1 = ghq_matrix1,
                  gradtol = nlm_params$gradtol,
                  stepmax = nlm_params$stepmax,
                  steptol = nlm_params$steptol,
                  hessian = TRUE,
                  print.level = nlm_params$verbose,
                  iterlim = 1000
  )
  return(loglik)
}

#' does LL calc
#' not used - C++ version is
#' @param working_values vector
#' @param model model list
#' @param processed data list
#' @param ghq_matrix1 mat
#' @returns loglik
#' @export
llCalc <- function(working_values,  model,  processed,  ghq_matrix1) {

  concept  <-  processed$concept
  data  <-  processed$data

  nmax_choiceset_size  <- processed$nmax_choiceset_size
  ndecisionmakers  <-  processed$ndecisionmakers

  npp <- model$npp
  nhop <- model$nhop

  integral_size <- dim(ghq_matrix1)[1]

  epsilonmatrix <- model$epsilon
  deltamatrix <- model$delta
  gammamatrix <- model$gamma
  deltamatrix <- model$delta
  betamatrix <- model$beta
  phimatrix <- model$phi
  code <- model$code

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
      muepsilonparameters[i] <- working_values[m]
    }

    if (epsilonmatrix[i, 1] == -1) muepsilonparameters[i] <- 1
  }

  for (i in 1:nhop) {

    if (deltamatrix[i, 1] == 1) {

      m <- m + 1
      mudeltaparameters[i] <- working_values[m]
    }

    if (deltamatrix[i, 1] == -1) {
      mudeltaparameters[i] <- 1
    }
  }


  for (i in 1:npp) {

    if (epsilonmatrix[i, 2] == 1) {
      m <- m + 1
      sigmaepsilonparameters[i] <- abs(working_values[m])
    }

    if (epsilonmatrix[i, 2] == -1) {
      sigmaepsilonparameters[i] <- 1
    }
  }


  for (i in 1:nhop) {

    if (deltamatrix[i, 2] == 1) {
      m <- m + 1
      sigmadeltaparameters[i] <- abs(working_values[m])
    }

    if (deltamatrix[i, 2] == -1) {
      sigmadeltaparameters[i] <- 1
    }
  }


  for (j in 1: nhop) {
    for (i in 1:npp) {
      if (gammamatrix[i, j] == 1) {
        m <- m + 1
        gammaparameters[i, j] <- working_values[m]
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
        betaparameters[i, j] <- working_values[m]
      }
      if (betamatrix[i, j] == -1) {
        betaparameters[i, j] <- 1
      }
    }
  }


  # diag(phiparameters) <-  1
  #
  #
  # for (i in 1:(npp + nhop - 1)) {
  #   for (j in (i + 1):(npp + nhop)) {
  #     if (phimatrix[i, j] == 1) {
  #       if (phimatrix[j, i] == 1) {
  #         m <- m + 1
  #         phiparameters[i, j] <- working_values[m]
  #         phiparameters[j, i] <- working_values[m]
  #       }
  #     }
  #   }
  #
  # ghq_matrix1 <- ghq_matrix1 %*% (phiparameters^0.5)

  #w1 <- ghq_matrix1[, 1]


  int_epsilon <- cbind(ghq_matrix1[, 2])

  if (npp > 1) {
    for (i in 3:(npp + 1)) {
      int_epsilon <- cbind(int_epsilon, ghq_matrix1[, i])
    }
  }

  int_delta <- cbind(ghq_matrix1[, npp + 2])

  if (nhop > 1) {
    for (i in 3:(nhop + 1)) {
      int_delta <- cbind(int_delta, ghq_matrix1[, i + npp])
    }
  }

  for (i in 1:integral_size) {
    for (j in 1:npp) {
      int_epsilon[i, j] <- int_epsilon[i, j] * sigmaepsilonparameters[j] + muepsilonparameters[j]
    }
    for (j in 1:nhop) {
      int_delta[i, j] <- int_delta[i, j] * sigmadeltaparameters[j] + mudeltaparameters[j]
    }
  }



  gb <- diag(nhop) - betaparameters

  gb <- solve(gb)

  gb  <-  gammaparameters %*% gb


  gb  <-  gb %*% t(int_delta)
  gb <- gb + t(int_epsilon)


  conceptuse <- concept %*% code
  gb <- conceptuse %*% gb


  gb <- exp(gb)

  pthisdm  <-  matrix(1, 1, integral_size)
  pthiscs  <-  matrix(0, 1, integral_size)

  ploglike  <-  array(0,  ndecisionmakers)
  n  <-  1
  iddm  <-  data[1, 1]
  bottom  <-  array(0,  ncol(gb))

  nlines  <-  dim(data)[1]

  for (i in 1:nlines) {
    bottom  <-  bottom * 0

    for (j in 1:nmax_choiceset_size) {
      if (data[i, j + 4] > 0) {
        bottom  <-  bottom + gb[data[i, j + 4], ]
      }
    }

    pthiscs  <-  matrix(gb[data[i, 2], ] / bottom, 1, integral_size)

    if (data[i, 1] == iddm) {
      pthisdm  <-  pthisdm * pthiscs
    }

    if (data[i, 1] > iddm) {
      ploglike[n] <- sum(pthisdm) / integral_size
      n  <-  n  +  1
      pthisdm  <-  pthiscs
      iddm  <-  data[i, 1]
    }
  }

  #print()
  ploglike[n] <- sum(pthisdm) / integral_size

  ploglike  <-  log(ploglike)
  loglike  <-  sum(ploglike)
  loglike  <-  -loglike

  return(loglike)
}
