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
  #ndecisionmakers  <-  processed$ndecisionmakers

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


  gb <- diag(nhop) - betaparameters
  gb <- solve(gb)
  gb  <-  gammaparameters %*% gb


  delta <- matrix(0, nrow = 1, ncol = length(mudeltaparameters))
  epsilon <- matrix(0, nrow = 1, ncol = length(muepsilonparameters))

  gb  <-  gb %*% t(delta)
  gb <- gb + t(epsilon)


  conceptuse <- concept %*% model$code
  gb <- conceptuse %*% gb
  gb <- exp(gb)


  nlines  <-  dim(data)[1]

  scores <-  matrix(0, nrow = nlines, ncol = dim(concept)[2])

  for (i in 1:nlines) {
    options <- data[i, (1:nmax_choiceset_size) + 4]

    options <- options[options > 0]
    probs <- gb[options, ]
    probs <- probs / sum(probs)
    choice <-  which(options == data[i, 2])
    d_i <- rep(0, length(probs))
    d_i[choice] <- 1

    scores[i, ] <- t(d_i - probs) %*% concept[options, ]
  }

  return(scores)
}


#' does sims
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

  groups <- as.numeric(factor(model$data$data[, 1]))

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




  data2 <- data
  nlines  <-  dim(data)[1]

  delta <-
    rmvnorm(ndecisionmakers,
            mudeltaparameters,
            diag(sigmadeltaparameters))
  epsilon <-
    rmvnorm(ndecisionmakers,
            muepsilonparameters,
            diag(sigmaepsilonparameters))

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
    sim_choice <- rmultinom(1, 1, probs)

    sim_choice_loc <- options[which.max(sim_choice)]
    data2[i, 2] <- sim_choice_loc

  }

  new_orig_data <-
    setNames(data.frame(matrix(
      ncol = 3 + length(processed$attribute_names),
      nrow = 0
    )),
    c("ID", "ChoiceSet", "Choice", processed$attribute_names))


  concept2  <- processed$concept
  colnames(concept2) <- processed$attribute_names

  choice_set_counter <- 0
  ID_check <- data2[1, 1]

  for (j in seq_len(nrow(data2))) {
    if (ID_check == data2[j, 1]) {
      choice_set_counter <- choice_set_counter + 1
    } else {
      choice_set_counter <- 1
      ID_check <- data2[j, 1]
    }

    choice_locator <-
      which(data2[j, (1:nmax_choiceset_size) + 4]  ==     data2[j, 2])




    for (k in 1:nmax_choiceset_size) {
      temp_list <-
        data.frame(matrix(c(
          data2[j, 1],
          choice_set_counter,
          1 * (choice_locator == k),
          concept2[data2[j, k + 4], ]
        ), nrow = 1))
      if (length(temp_list) > 3) {
        names(temp_list) <- names(new_orig_data)
        new_orig_data <- bind_rows(new_orig_data, temp_list)
      }
    }

  }

  return(new_orig_data)
}

#' simualte data
#' @param processed data to base simulations on
#' @param model_type model_type
#' @param chosen_values sim params
#' @param easy_guess guess true values
#' @returns sims
#' @export
simulate_dataset <-
  function(processed,
           model_type,
           chosen_values,
           easy_guess = FALSE) {
    #processed <- setUp(template)
    model <- model_generator(processed, model_type)
    test_sims <- simulation(chosen_values,  model,  processed)



    processed_sims <- setUp(test_sims)
    model_sims <- model_generator(processed_sims, model_type)

    if (easy_guess) {
      model_sims$initial_values <- chosen_values
    }

    return(model_sims)
  }


#' estimate things
#' @param model_sims simulation to estimate from
#' @param type estimation method
#' @param precision precision parameter
#' @returns results
#' @export
estimate_model <- function(model_sims, type, precision) {
  if (type == "ghq") {
    dev_mode <- "C"
    ghq_size <- precision

    tryCatch(
      expr = {
        results <-
          runModel(
            model_sims,
            dev_mode = dev_mode,
            ghq_size = ghq_size,
            verbose = 0
          )
        return(results)
      },
      error = function(e) {
        return(NA)
      }
    )

  } else if (type == "draws") {
    dev_mode <- "Cdraws"
    draws <- precision

    tryCatch(
      expr = {
        results <-
          runModel(
            model_sims,
            dev_mode = dev_mode,
            draws = draws,
            verbose = 0
          )
        return(results)
      },
      error = function(e) {
        return(NA)
      }
    )


  } else if (type == "TMB") {
    tryCatch(
      expr = {
        results <- runModel(model_sims)
        return(results)
      },
      error = function(e) {
        return(NA)
      }
    )


  }


}

#' creates structure for simulations
#' @param m sample size
#' @param p variables
#' @returns results
#' @export
generate_simulation_templates <- function(m, p = 2) {
  #-----------------------------
  # define attributes and levels
  #-----------------------------
  desVarNames <- LETTERS[1:p]
  desLevels <- rep(4, p)
  n <- p      #number of choice sets
  desOpt <- 4  #num option per choice set
  #generate full factorial
  dat <-
    AlgDesign::gen.factorial(desLevels,
                             length(desLevels),
                             varNames = desVarNames,
                             center = TRUE)

  destT <-
    AlgDesign::optFederov(~ ., dat, nTrials = (n * (desOpt)), criterion = "D")
  design_ouput <- destT$design * 0.2

  list_df <- list()

  for (i in 1:m) {
    list_df[[i]] <- data.frame(
      ID = 1000 + i,
      ChoiceSet = rep(seq_len(n), each = desOpt),
      Choice = sample(c(rep(0, desOpt - 1), 1), desOpt),
      design_ouput
    )
    names(list_df[[i]]) <-
      c("ID", "ChoiceSet", "Choice", paste0(LETTERS[seq_len(p)], "_DCE"))
  }
  template_DCE <- bind_rows(list_df)
  row.names(template_DCE) <- NULL


  l <- length(desLevels) + 1

  design_ouput2 <- crossdes::find.BIB(l, 4, 3, iter = 30)

  combinations <- combn(seq_len(ncol(design_ouput2)), 2)

  res <-
    matrix(0,
           nrow = nrow(design_ouput2) * ncol(combinations),
           ncol = l)

  k <- 1

  for (i in seq_len(nrow(design_ouput2))) {
    for (j in seq_len(ncol(combinations))) {
      res[k, design_ouput2[i, combinations[1, j]]] <- 1
      res[k, design_ouput2[i, combinations[2, j]]] <- -1
      k <- k + 1
    }
  }

  desOpt2 <- ncol(combinations)

  for (i in 1:m) {
    list_df[[i]] <- data.frame(
      ID =  rep(1000 + i, 12),
      ChoiceSet = rep(seq_len(nrow(design_ouput2)), each = desOpt2),
      Choice = sample(c(rep(0, desOpt2 - 1), 1), desOpt2),
      res
    )
    names(list_df[[i]]) <-
      c("ID", "ChoiceSet", "Choice", paste0(LETTERS[1:l], "_BW"))

  }

  template_BW <- bind_rows(list_df)
  row.names(template_BW) <- NULL

  #template_BW2 <- template_BW[, seq_len(3 + p)]


  processed_template_BW <- setUp(template_BW)
  name <-
    processed_template_BW$attribute_names[length(processed_template_BW$attribute_names)]
  processed_template_BW  <-
    remove_variables(processed_template_BW, name)
  processed_template_DCE <- setUp(template_DCE)
  processed_template_BWDCE  <-
    join_choicedatasets(processed_template_BW, processed_template_DCE)

  processed <- list()

  processed[["DCE"]] <- processed_template_DCE
  processed[["BW"]] <- processed_template_BW
  processed[["BWDCE"]] <- processed_template_BWDCE

  return(processed)
}

#' runs a batch of simulations
#' @param data_sets list of dataset types
#' @param chosen_values simulation params
#' @param precision_levels for estimates
#' @param integral_types estimation method
#' @param models model types to test
#' @param m_list list of sample sizes for sims
#' @param n_sims no of sims
#' @param file outfile for save
#' @returns list of results
#' @export
run_sims <- function(data_sets,
                     chosen_values,
                     precision_levels,
                     integral_types,
                     models,
                     m_list,
                     n_sims,
                     p = 2,
                     file = NULL) {
  params <- list(
    data_sets = data_sets,
    chosen_values = chosen_values,
    precision_levels = precision_levels,
    integral_types = integral_types,
    models = models,
    m_list = m_list,
    n_sims = n_sims,
    p = p
  )

  big_list <- list()

  for (m in m_list) {
    m_size <- paste0("m_", m)

    processed <- generate_simulation_templates(m, p)

    for (model_type in models) {
      for (data_type in data_sets) {
        for (eg in c(TRUE, FALSE)) {
          eg_name <- paste0("eg_", eg)

          big_list[[m_size]][[model_type]][[data_type]][["specs"]] <-
            list(
              values = chosen_values[[model_type]][[data_type]],
              n_sims = n_sims,
              m = m,
              easy_guess = eg,
              template = processed[[data_type]]
            )

          if (!is.na(chosen_values[[model_type]][[data_type]][1])) {
            for (i in 1:n_sims) {
              n_name <- paste0("n_", i)
              message(paste(m_size, model_type, data_type, eg_name, i))

              big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][[n_name]][["sim"]] <-
                simulate_dataset(processed[[data_type]],
                                 model_type,
                                 chosen_values[[model_type]][[data_type]],
                                 easy_guess = eg)

              for (g in integral_types) {
                for (p in precision_levels[[g]]) {
                  p_name <- paste0("p_", p)
                  message(paste(".   sim:", i, g, p_name))
                  temp_sim <- big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][[n_name]][["sim"]]
                  big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][[n_name]][[g]][[p_name]] <-
                    estimate_model(temp_sim, g, p)
                }
              }
            }
            if (!is.null(file)) {
              save(big_list, params, file = file)
            }
            gc()
          }
        }
      }
    }
  }
  return(big_list)
}


#' cleans up data batch of simulations. params need to mathc those used for sim
#' @param data_sets list of dataset types
#' @param chosen_values simulation params
#' @param precision_levels for estimates
#' @param integral_types estimation method
#' @param models model types to test
#' @param m_list list of sample sizes for sims
#' @param n_sims no of sims
#' @returns list of results
#' @export
process_sims <-
  function(big_list,
           data_sets,
           chosen_values,
           precision_levels,
           integral_types,
           models,
           m_list,
           n_sims,
           conf_level = 0.95) {
    alpha <- 1 - (1 - conf_level) / 2
    critical_val <- qnorm(alpha)

    row_count <- 1
    row_list <- list()

    for (m in m_list) {
      m_size <- paste0("m_", m)
      for (model_type in models) {
        for (data_type in data_sets) {
          for (eg in c(TRUE, FALSE)) {
            eg_name <- paste0("eg_", eg)

            if (!is.na(chosen_values[[model_type]][[data_type]][1])) {
              true <- big_list[[m_size]][[model_type]][[data_type]][["specs"]]$values
              for_labs <- big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][["n_1"]][["sim"]]
              names <- parameter_labels(for_labs)
              estimates <- list()
              standard_errors <- list()
              times <- list()

              for (g in integral_types) {
                for (p in precision_levels[[g]]) {
                  p_name <- paste0("p_", p)
                  for (i in 1:n_sims) {
                    n_name <- paste0("n_", i)
                    na_check <-
                      big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][[n_name]][[g]][[p_name]][1]
                    if (!is.na(na_check)) {
                      temp_result <-
                        big_list[[m_size]][[model_type]][[data_type]][[eg_name]][["results"]][[n_name]][[g]][[p_name]]
                      estimates[[i]] <- temp_result$results$estimate
                      standard_errors[[i]] <-
                        temp_result$results$standard_errors
                      times[[i]] <-
                        temp_result$execution_time
                    } else {
                      estimates[[i]] <- true * NA
                      standard_errors[[i]] <- true * NA
                      times[[i]] <- NA
                    }

                  }
                  for (j in seq_len(length(true))) {
                    estimates_j <- unlist(lapply(estimates, "[[", j))
                    true_j <- true[j]
                    standard_errors_j <-
                      unlist(lapply(standard_errors,  "[[", j))

                    mu <- mean(estimates_j)
                    std_deviations <-
                      mean((estimates_j - mu) ^ 2, na.rm = TRUE)
                    z_scores <-
                      (estimates_j - true_j) / standard_errors_j

                    upper_bounds <-
                      estimates_j + critical_val * standard_errors_j
                    lower_bounds <-
                      estimates_j - critical_val * standard_errors_j

                    sw_p_value <- if (sum(is.finite(z_scores)) > 3) {
                      shapiro.test(z_scores)$p.value
                    } else {
                      NA
                    }
                    coverage_probability <-
                      (sum(1.0 * ((true_j > lower_bounds) & (true_j < upper_bounds)),
                           na.rm = TRUE) / length(estimates))

                    row_list[[row_count]] <- data.frame(
                      m_size = m_size,
                      data_type = data_type,
                      model_type = model_type,
                      integral_type = g,
                      precision = p,
                      n = length(estimates),
                      time = mean(unlist(times), na.rm = TRUE),
                      start = eg_name,
                      true = true_j,
                      name = names[j],
                      bias = mean(estimates_j - true_j, na.rm = TRUE),
                      coverage_probability = coverage_probability,
                      mse = mean((estimates_j - true_j) ^ 2, na.rm = TRUE),
                      mean_std_deviation = mean(std_deviations, na.rm = TRUE),
                      bias_pc = 100 * mean(estimates_j - true_j, na.rm = TRUE) / true_j,
                      mu = mean(estimates_j, na.rm = TRUE),
                      sw_p_value = sw_p_value,
                      good_estimate = coverage_probability > 0.90 &
                        sw_p_value > 0.01
                    )
                    row_count <- row_count + 1
                  }
                }
              }
            }
          }
        }
      }
    }
    results_table <- bind_rows(row_list)
    return(results_table)
  }
