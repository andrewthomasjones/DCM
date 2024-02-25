#' Runs Model
#' @param model model list
#' @param model_name string default "name"
#' @param verbose default 0
#' @param gradtol default 1e-6
#' @param stepmax default as in nlm
#' @param steptol default 1e-6
#' @param dev_mode default "orig"
#' @param ghq_size 3
#' @param draws 100 fro now when needed
#' @returns fitted model.
#' @export
runModel  <-  function(model,  model_name = "name", verbose = 0,
                       gradtol = 1e-6, stepmax = NULL, steptol = 1e-6,
                       dev_mode = "C", ghq_size = 3, draws = 100) {

  parcount <- parameterCount(model)
  processed <- model$data

  model_name <- model$description

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

  nrc <- dim(model$epsilon)[1] + dim(model$delta)[1]


  if (is.null(stepmax)) {
    stepmax <- max(1000 * sqrt(sum((model$initial_values)^2)), 1000)

  }

  #pass through in case these need to be accessed
  nlm_params <- list(
    gradtol = gradtol,
    stepmax = stepmax,
    steptol = steptol,
    verbose = verbose
  )

  delta_grid <- suppressMessages(mvQuad::createNIGrid(dim = nhop + npp,
                                                      type = "GHN",
                                                      level = ghq_size,
                                                      ndConstruction = "sparse"))
  ghq_matrix1 <- as.matrix(cbind(delta_grid$weights, delta_grid$nodes))


  ghq_steps <- draws
  shuffle <- TRUE
  gq_int_matrix <- gqIntMatrix(ghq_steps, nrc, shuffle)
  weights <- rep(1 / (ghq_steps), ghq_steps)
  ghq_matrix2 <- as.matrix(cbind(weights, gq_int_matrix))

  if (dev_mode == "C") {
    loglik1 <- suppressWarnings(llMax_ghq(model,  processed,  ghq_matrix1, nlm_params))
  }else if (dev_mode == "R") {
    loglik1 <- suppressWarnings(llMax(model,  processed,  ghq_matrix1, nlm_params))
  }else if (dev_mode == "Cdraws") {
    loglik1 <- suppressWarnings(llMax_ghq(model,  processed,  ghq_matrix2, nlm_params))
  }else if (dev_mode == "Rdraws") {
    loglik1 <- suppressWarnings(llMax(model,  processed,  ghq_matrix2, nlm_params))
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

  results$LL  <-   c(loglik1$minimum,  rep(".",  nrow(results) - 1))

  result_name <- paste0(model_name,  " ",
                        format(Sys.time(),
                               "%Y-%m-%d %H:%M"))

  fitted_model  <-  list(result_name = result_name,
                         model = model,
                         model_name = model_name,
                         LL = loglik1$minimum,
                         loglikf = loglik1,
                         results = results,
                         AIC = AIC,
                         BIC = BIC,
                         par_count = parcount
  )

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
