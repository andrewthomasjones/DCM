choice_picker <- function(data){
  choices <- data[,2]
  slots <- data[, 5:ncol(data)]
  locations <-  unlist(mapply(function(x, a){which(slots[x, ] == a)[1]}, seq_len(nrow(slots)), choices)) #why do some rows in data matrix have repeats??

  d <- slots*0
  for(i in seq_len(nrow(slots))){
    d[i, locations[i]] <- 1
  }


  return(d)
}


run_model_TMB <- function(model){

  start_time <- Sys.time()

  processed_data <- model$data

  model_type <- model$description

  parameters_labels  <-  parameter_labels(model)

  data <- list(concept = model$data$concept,
               data = model$data$data[, 5:ncol(model$data$data)],
               code = model$code,
               group = factor(model$data$data[,1]),
               choices = choice_picker(model$data$data), #matrix of choices - each row all zeros but has one 1
               imatrix = diag(model$nhop)
  )


  model$gamma[model$gamma == -1] <- 1
  model$beta[model$beta == -1] <- 1
  model$delta[model$delta == -1] <- 1

  parameters <- list(
    muepsilon = matrix(model$epsilon[, 1], nrow = 1),
    logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1)),
    mudelta = matrix(model$delta[, 1], nrow = 1),
    logsigmadelta = log(matrix(model$delta[, 2], nrow = 1)),
    gamma = model$gamma,
    beta = model$beta,
    phi = model$phi,
    epsilon = matrix(rnorm(model$data$ndecisionmakers*model$npp), nrow = model$data$ndecisionmakers, ncol = model$npp, byrow=TRUE),
    delta = matrix(rnorm(model$data$ndecisionmakers*model$nhop), nrow = model$data$ndecisionmakers, ncol = model$nhop, byrow=TRUE)
  )

  random <- c("epsilon", "delta")

    map <- list(
      muepsilon = matrix(model$epsilon[, 1], nrow = 1)*NA,
      logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1))*NA,
      mudelta = model$delta[, 1]*NA,
      logsigmadelta = log(matrix(model$delta[, 2], nrow = 1))*NA,
      gamma = model$gamma*NA,
      beta = model$beta*NA,
      phi = model$phi*NA
     )

  if(model_type == "fixed"){

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))
    parameters$logsigmaepsilon[1,] <- log(1e-8)

  }else if (model_type == "random"){
    parameters$logsigmadelta[1,] <- 0
    parameters$logsigmaepsilon[1,] <- log(1e-8)

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))

    map$logsigmadelta <- paste0("logsigma_delta_", seq_len(length(model$epsilon[, 2])))

  }else if (model_type == "one-factor"){

    gamma1 <- model$gamma
    gamma1[model$gamma == 0] <- NA
    gamma1[!is.na(gamma1)] <- paste0("gamma_", seq_len(sum(!is.na(gamma1))))

    map$gamma <- gamma1

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))
    parameters$logsigmaepsilon[1,] <- log(1e-8)

  }else if (model_type == "mtmm"){

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))

    parameters$logsigmaepsilon[1,] <- log(1e-8)

    gamma1 <- model$gamma
    gamma1[model$gamma == 0] <- NA
    gamma1[!is.na(gamma1)] <- paste0("gamma_", seq_len(sum(!is.na(gamma1))))

    beta1 <- model$beta
    beta1[model$beta==0] <- NA
    beta1[!is.na(beta1)] <- paste0("beta_", seq_len(sum(!is.na(beta1))))

    map$gamma <- gamma1
    map$beta <- beta1

  }

  map_f <- lapply(map, as.factor)


  # instantiate ModelA object
  obj <- TMB::MakeADFun(data = c(model = "DCMLL", # which model to use
                                  data),
                         parameters = parameters,
                         map = map_f,
                         random = random,
                         hessian = TRUE,
                         silent=TRUE,
                         DLL = "DCM_TMBExports") # package's DLL

  #obj$env$tracepar <- TRUE

  ## Test eval function and gradient
  fn <- obj$fn(obj$par)
  gr <- obj$gr(obj$par)

  upper_lims = Inf
  lower_lims = -Inf

  ## Fit model
  opt <- nlminb(obj$par, obj$fn, obj$gr, upper = upper_lims, lower = lower_lims)

  rep <- TMB::sdreport(obj, bias.correct = TRUE)
  se <- summary(rep, "fixed")

  result_name <- paste0(model_type,  " ",
                        format(Sys.time(),
                               "%Y-%m-%d %H:%M"))
  K <-  length(obj$par)
  LL <- opt$objective

  results  <-  data.frame(parameters = parameters_labels, #FIXME order is wrong
                          estimate = opt$par,
                          standard_errors = se[,2])

  end_time <- Sys.time()
  time_taken <- end_time - start_time

  fitted_model  <-  list(result_name = result_name,
                         model = model,
                         model_name = model_type,
                         LL = opt$objective,
                         loglikf = opt,
                         results = results,
                         AIC = 2*K - 2*LL,
                         BIC = -2*LL + K*log(nrow(model$data$data)),
                         par_count = K,
                         execution_time = as.numeric(time_taken)
  )

  return(fitted_model)

}



