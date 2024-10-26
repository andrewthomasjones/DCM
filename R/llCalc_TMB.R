choice_picker <- function(data) {
  choices <- data[, 2]
  slots <- data[, 5:ncol(data)]
  locations <-
    unlist(mapply(function(x, a) {
      which(slots[x, ] == a)[1]
    },
    seq_len(nrow(slots)), choices))
  d <- slots * 0
  for (i in seq_len(nrow(slots))) {
    d[i, locations[i]] <- 1
  }


  return(d)
}

#' Runs Model
#' @param model model list
#' @param verbose default FALSE
#' @returns fitted model.
#' @export
run_model_TMB <- function(model, verbose = FALSE) {

  start_time <- Sys.time()

  #processed_data <- model$data

  model_type <- model$description

  parameters_labels  <-  parameterLabels(model)

  data <- list(concept = model$data$concept,
               data = model$data$data[, 5:ncol(model$data$data)],
               code = model$code,
               group = factor(model$data$data[, 1]),
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
    epsilon = matrix(rnorm(model$data$ndecisionmakers * model$npp),
                     nrow = model$data$ndecisionmakers, ncol = model$npp, byrow = TRUE),
    delta = matrix(rnorm(model$data$ndecisionmakers * model$nhop),
                   nrow = model$data$ndecisionmakers, ncol = model$nhop, byrow = TRUE)
  )

  random <- c("epsilon", "delta")

  map <- list(
    muepsilon = matrix(model$epsilon[, 1], nrow = 1) * NA,
    logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1)) * NA,
    mudelta = model$delta[, 1] * NA,
    logsigmadelta = log(matrix(model$delta[, 2], nrow = 1)) * NA,
    gamma = model$gamma * NA,
    beta = model$beta * NA,
    phi = model$phi * NA
  )

  if (model_type == "fixed") {

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))
    parameters$logsigmaepsilon[1, ] <- log(1e-8)

  }else if (model_type == "random") {
    parameters$logsigmadelta[1, ] <- 0
    parameters$logsigmaepsilon[1, ] <- log(1e-8)

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))

    map$logsigmadelta <- paste0("logsigma_delta_", seq_len(length(model$epsilon[, 2])))

  }else if (model_type == "one-factor") {

    gamma1 <- model$gamma
    gamma1[model$gamma == 0] <- NA
    gamma1[!is.na(gamma1)] <- paste0("gamma_", seq_len(sum(!is.na(gamma1))))

    map$gamma <- gamma1

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))
    parameters$logsigmaepsilon[1, ] <- log(1e-8)

  }else if (model_type == "mtmm") {

    map$muepsilon <- paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1])))

    parameters$logsigmaepsilon[1, ] <- log(1e-8)

    gamma1 <- model$gamma
    gamma1[model$gamma == 0] <- NA
    gamma1[!is.na(gamma1)] <- paste0("gamma_", seq_len(sum(!is.na(gamma1))))

    beta1 <- model$beta
    beta1[model$beta == 0] <- NA
    beta1[!is.na(beta1)] <- paste0("beta_", seq_len(sum(!is.na(beta1))))

    map$gamma <- gamma1
    map$beta <- beta1

  }

  map_f <- lapply(map, as.factor)

  if (verbose) {
    cli::cli_inform("Setting up TMB ADFun...")
  }

  # instantiate ModelA object
  obj <- TMB::MakeADFun(data = c(model = "DCMLL", # which model to use
                                 data),
                        parameters = parameters,
                        map = map_f,
                        random = random,
                        hessian = TRUE,
                        silent = TRUE,
                        DLL = "DCM_TMBExports") # package's DLL

  #obj$env$tracepar <- TRUE

  ## Test eval function and gradient
  #fn <- obj$fn(obj$par)
  #gr <- obj$gr(obj$par)

  upper_lims <- Inf
  lower_lims <- -Inf

  ## Fit model
  opt <- nlminb(obj$par, obj$fn, obj$gr, upper = upper_lims, lower = lower_lims)

  if (verbose) {
    cli::cli_inform("Running TMB SD report...")
  }

  rep <- TMB::sdreport(obj, bias.correct = TRUE, getReportCovariance = FALSE, bias.correct.control = list(sd = TRUE))


  se <- summary(rep, "fixed")
  se2 <- summary(rep, "report")

  se_final <- se

  logged_params <- stringr::str_detect(row.names(se_final), "^log")
  match_names <- stringr::str_replace(row.names(se_final)[logged_params], "log", "")
  swap_idx <- which(row.names(se2) %in% match_names)

  se_final[swap_idx, 1:2] <-  se2[swap_idx, 1:2]
  row.names(se_final)[swap_idx] <- row.names(se2)[swap_idx]

  result_name <- paste0(model_type,  " ",
                        format(Sys.time(),
                               "%Y-%m-%d %H:%M"))
  K <-  length(obj$par)
  LL <- opt$objective

  clean_names_old <- stringr::str_replace(parameters_labels, "[^[:alpha:]]+", "")
  clean_names_new <- row.names(se_final)
  clean_names_new <- stringr::str_replace(clean_names_new, "mu", "")

  sorting_frame <- data.frame(clean_names_old = clean_names_old,
                              clean_names_new = clean_names_new,
                              frame_order = seq_len(nrow(se_final)))

  # sorting_frame <- sorting_frame %>%
  #   group_by(clean_names_old) %>%
  #   mutate(order_old = row_number()) %>%
  #   ungroup()

  sorting_frame$order_old  <- stats::ave(sorting_frame$frame_order, sorting_frame$clean_names_old, FUN = seq_along)

  # sorting_frame <- sorting_frame %>%
  #   group_by(clean_names_new) %>%
  #   mutate(order_new = row_number()) %>%
  #   ungroup()

  sorting_frame$order_new  <- stats::ave(sorting_frame$frame_order, sorting_frame$clean_names_new, FUN = seq_along)

  sorting_frame$clean_names_new2 <- factor(clean_names_new, levels = unique(clean_names_old))
  sorting_frame <- sorting_frame[order(sorting_frame$clean_names_new2, sorting_frame$order_new), ]




  variable_names <- array(NA, length(parameters_labels))
  parameters_label_idx <- stringr::str_match(parameters_labels, "^([a-z]{1,20})_\\[([0-9]{1,3}),")

  for(i in seq_len(length(parameters_labels))){
    variable_names[i] <- row.names(model[[parameters_label_idx[i,2]]])[as.numeric(parameters_label_idx[i,3])]
  }



  results  <-  data.frame(variable = variable_names,
                          parameter = parameters_labels, #FIXME order is wrong
                          estimate = se_final[sorting_frame$frame_order, 1],
                          standard_error = se_final[sorting_frame$frame_order, 2])

  results$LL  <-   c(opt$objective,  rep(".",  nrow(results) - 1))

  end_time <- Sys.time()
  time_taken <- end_time - start_time

  fitted_model  <-  list(result_name = result_name,
                         model = model,
                         model_name = model_type,
                         LL = opt$objective,
                         loglikf = opt,
                         results = results,
                         AIC = 2 * K - 2 * LL,
                         BIC = -2 * LL + K * log(nrow(model$data$data)),
                         par_count = K,
                         execution_time = as.numeric(time_taken)
  )

  return(fitted_model)

}
