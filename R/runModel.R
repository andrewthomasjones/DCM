#' Runs Model
#' @param model model list
#' @param verbose default 0
#' @param gradtol default 1e-6
#' @param stepmax default as in nlm
#' @param steptol default 1e-6
#' @param integral_type NULL
#' @param ghq_size NUL
#' @param draws NULL
#' @returns fitted model.
#' @export
runModel  <-  function(model,  verbose = 0,
                       gradtol = 1e-6, stepmax = NULL, steptol = 1e-6,
                       integral_type = NULL, ghq_size = NULL, draws = NULL) {

  #start timer
  start_time <- Sys.time()

  #basic data properties
  parcount <- parameterCount(model)
  processed <- model$data
  model_type <- model$description

  #defaults for various things if entered as NULL
  if (model_type == "mtmm" && is.null(integral_type)) {
    integral_type <- "Draws"
  } else if (model_type == "manual" && is.null(integral_type)) {
    integral_type <- "GHQ"
  } else if (is.null(integral_type)) {
    integral_type <- "TMB"
  }

  if (is.null(draws)) {
    draws <-  1000
  }

  if (integral_type == "GHQ" && is.null(ghq_size)) {
    ghq_size <- 3
  }

  npp <- model$npp
  nhop <- model$nhop

  if (length(model$initial_values) == parcount$total) {
    if (verbose > 0) {
      message("You have the correct number of initial values.")
    }

  }else {
    stop("ERROR - you have the incorrect number of initial values.")
    return(NA)
  }


  if (model_type == "mtmm") {
    data_names <- stringr::str_match(model$data$attribute_names, "^([[:alnum:]]{1,})_([[:alnum:]]{1,})$")

    if (length(unique(data_names[, 3])) < 2) {
      stop("Only one sub-dataset, MTMM model is not appropriate")
    }

    unique_name_length <- length(unique(data_names[, 2])) * length(unique(data_names[, 3]))
    if (unique_name_length != length(model$data$attribute_names)) {
      stop("Naming inconsistent.")
    }

  }

  nrc <- dim(model$epsilon)[1] + dim(model$delta)[1]


  if (is.null(stepmax)) {
    stepmax <- max(1000 * sqrt(sum((model$initial_values)^2)), 1000)

  }


  if (integral_type == "TMB") {
    fitted_model <- run_model_TMB(model)
  }else {
    #pass through in case these need to be accessed
    nlm_params <- list(
      gradtol = gradtol,
      stepmax = stepmax,
      steptol = steptol,
      verbose = verbose
    )


    if (integral_type == "GHQ") {
      delta_grid <- suppressMessages(mvQuad::createNIGrid(dim = nhop + npp,
                                                          type = "GHe",
                                                          level = ghq_size,
                                                          ndConstruction = "sparse"))

      ghq_matrix1 <- as.matrix(cbind(delta_grid$weights, delta_grid$nodes))

    }else if (integral_type == "Draws") {
      shuffle <- TRUE
      gq_int_matrix <- gqIntMatrix(draws, nrc, shuffle)
      weights <- rep(1 / (draws), draws)
      ghq_matrix2 <- as.matrix(cbind(weights, gq_int_matrix))
    }

    if (integral_type == "GHQ") {
      loglik1 <- suppressWarnings(llMax_ghq(model,  processed,  ghq_matrix1, nlm_params))
    }else if (integral_type == "Draws") {
      loglik1 <- suppressWarnings(llMax_ghq(model,  processed,  ghq_matrix2, nlm_params))
    }

    #print(loglik1$hessian)
    #standard_errors  <- array(NA, length(loglik1$estimate))
    standard_errors  <-  sqrt(diag(solve(loglik1$hessian)))


    printpara  <-  matrix(0,  7,  1)
    row.names(printpara)  <-  c("epsilon_mu",
                                "epsilon_sig",
                                "delta_mu",
                                "delta_sig",
                                "gamma",
                                "beta",
                                "phi")

    for (i in 1:npp) {
      if (model$epsilon[i, 1] == 1) {
        printpara[1] <- printpara[1] + 1
      }
      if (model$epsilon[i, 2] == 1) {
        printpara[2] <- printpara[2] + 1
      }
    }

    for (i in 1:nhop) {
      if (model$delta[i, 1] == 1) {
        printpara[3] <- printpara[3] + 1
      }
      if (model$delta[i, 2] == 1) {
        printpara[4] <- printpara[4] + 1
      }
    }

    for (i in 1:npp) {
      for (j in 1:nhop) {
        if (model$gamma[i, j] == 1) {
          printpara[5] <- printpara[5] + 1
        }
      }
    }

    for (i in 1:nhop) {
      for (j in 1:nhop) {
        if (model$beta[i, j] == 1) {
          printpara[6] <- printpara[6] + 1
        }
      }
    }

    k <- parcount$total
    n <- nrow(processed$data)
    AIC  <-  2 * k - 2 * log(loglik1$minimum)
    BIC  <-  k * log(n) - 2 * log(loglik1$minimum)


    para_stems  <-  c(rep(("epsilon"),
                          printpara[1]),
                      rep(("epsilon_sig"),
                          printpara[2]),
                      rep(("delta_mu"),
                          printpara[3]),
                      rep(("delta_sig"),
                          printpara[4]),
                      rep(("gamma"),
                          printpara[5]),
                      rep(("beta"),
                          printpara[6]))

    subscripts  <-  matrix(rbind(which(model$epsilon > 0,  arr.ind = TRUE),
                                 which(model$delta > 0,  arr.ind = TRUE),
                                 which(model$gamma > 0,  arr.ind = TRUE),
                                 which(model$beta > 0,  arr.ind = TRUE)),
                           nrow = sum(printpara),  ncol = 2)

    parameters  <-  paste0(para_stems, "_",
                           "[", subscripts[, 1], ", ", subscripts[, 2], "]")


    results  <-  data.frame(parameters = parameters,
                            estimate = loglik1$estimate,
                            standard_errors = standard_errors)

    #make sure standard deviation estimates corrected to be positive
    results$estimate[stringr::str_detect(results$parameters, "_sig_")] <-
      abs(results$estimate[stringr::str_detect(results$parameters, "_sig_")])


    results$LL  <-   c(loglik1$minimum,  rep(".",  nrow(results) - 1))

    result_name <- paste0(model_type,  " ",
                          format(Sys.time(),
                                 "%Y-%m-%d %H:%M"))

    end_time <- Sys.time()
    time_taken <- end_time - start_time

    fitted_model  <-  list(result_name = result_name,
                           model = model,
                           model_type = model_type,
                           LL = loglik1$minimum,
                           loglikf = loglik1,
                           results = results,
                           AIC = AIC,
                           BIC = BIC,
                           par_count = parcount,
                           execution_time = as.numeric(time_taken)
    )
  }

  return(fitted_model)
}

#' Integral matrix
#' @param integral_size int number steps for integral using Gaussian quadrature
#' @param nrc int number of columns
#' @param shuffle default true
#' @returns integral matrix
gqIntMatrix <- function(integral_size, nrc, shuffle = TRUE) {

  int_range <- (1:(integral_size)) / (integral_size + 1)
  q <- qnorm(int_range)
  int_mat <- matrix(rep(q, nrc), integral_size, nrc)

  if (shuffle) {
    int_mat <- apply(int_mat, 2, sample)
  }
  return(int_mat)

}
