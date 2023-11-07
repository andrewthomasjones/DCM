#' create_emi_file
#'
#' @param file_name processed data list
#' @param model_type type for base model string
#' @param ncovariates need this or name list
#' @param attribute_names need this ncovariates
#' @param verbose prints more if >0
#' @returns creates file
#' @export
create_emi_file  <-  function(file_name,  model_type,  ncovariates = NULL,  attribute_names = NULL,  verbose = 1)  {

  #various checks

  model_types  <-  c("fixed",   "random",   "one-factor",  "mtmm")

  if (!(model_type %in% model_types)) {
    stop(paste("model_type not one of ",   paste(model_types,   collapse = ",   ")))
  }

  if (is.null(ncovariates) && is.null(attribute_names)) {
    stop("Need to provide at least one of ncovariates and attribute_names")
  }

  if (is.null(ncovariates) && !is.null(attribute_names)) {
    ncovariates <- length(attribute_names)
  }

  if (is.null(attribute_names) && !is.null(ncovariates)) {
    attribute_names <- paste0("V",  1:ncovariates)
  }

  if (!is.null(attribute_names) && !is.null(ncovariates)) {
    if (length(attribute_names) != ncovariates) {
      stop("attribute_names length must equal ncovariates")
    }
  }


  wb <- openxlsx::createWorkbook(file_name)

  if (model_type == "fixed") {

    openxlsx::addWorksheet(wb,  "epsilon_model")
    default_muep  <- rep(1, ncovariates)
    default_musig <- rep(0, ncovariates)
    epsilon_model <- matrix(cbind(default_muep, default_musig),  ncol = 2,
                            dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "epsilon_model",  epsilon_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_model")
    default_deltaep   <- 0
    default_deltasig  <- -1
    delta_model       <- matrix(cbind(default_deltaep, default_deltasig),  ncol = 2,
                                dimnames = list("HoP_1", c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "delta_model",  delta_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_model")
    default_gamma   <- rep(0, ncovariates)
    gamma_model     <- matrix(default_gamma,  ncol = 1,  dimnames = list(attribute_names, "HoP_1"))
    openxlsx::writeData(wb,  gamma_model,  sheet = "gamma_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_model")
    default_beta    <- 0
    beta_model      <- matrix(default_beta,  ncol = 1,  dimnames = list("HoP_1", "HoP_1"))
    openxlsx::writeData(wb,  beta_model,  sheet = "beta_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "epsilon_initialvalues")
    default_muep_initial   <- rep(.1, ncovariates)
    default_musig_initial  <- rep(NA, ncovariates)
    epsilon_model_initial  <- matrix(cbind(default_muep_initial, default_musig_initial),
                                     ncol = 2,  dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  epsilon_model_initial,  sheet = "epsilon_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_initialvalues")
    default_deltaep_initial   <- rep(NA, 1)
    default_deltasig_initial  <- rep(NA, 1)
    delta_model_initial <- matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                  ncol = 2,  dimnames = list("HoP_1", c("mu", "sigma")))
    openxlsx::writeData(wb,  delta_model_initial,  sheet = "delta_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_initialvalues")
    default_gamma_initial  <- rep(NA, ncovariates)
    gamma_model_initial    <- matrix(default_gamma_initial, ncol = 1,  dimnames = list(attribute_names, "HoP_1"))
    openxlsx::writeData(wb,  gamma_model_initial,  sheet = "gamma_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_initialvalues")
    default_beta_initial   <- rep(NA, 1)
    beta_model_initial  <- matrix(default_beta_initial, ncol = 1,  dimnames = list("HoP_1", "HoP_1"))
    openxlsx::writeData(wb,  beta_model_initial,  sheet = "beta_initialvalues",  rowNames = TRUE)

  }else if (model_type == "random") {

    openxlsx::addWorksheet(wb,  "epsilon_model")
    default_muep  <- rep(1, ncovariates)
    default_musig <- rep(0, ncovariates)
    epsilon_model <- matrix(cbind(default_muep, default_musig),  ncol = 2,
                            dimnames = list(attribute_names,  c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "epsilon_model",  epsilon_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_model")
    nhop <- ncovariates
    default_deltaep   <- rep(0, nhop)
    default_deltasig  <- rep(1, nhop)
    delta_model       <- matrix(cbind(default_deltaep, default_deltasig),  ncol = 2,
                                dimnames = list(paste("HoP_", 1:nhop, sep = ""),  c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "delta_model",  delta_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_model")
    default_gamma   <- diag(nhop) * -1
    gamma_model     <- matrix(default_gamma, ncol = nhop,
                              dimnames = list(attribute_names,  paste("HoP_", 1:nhop, sep = "")))
    openxlsx::writeData(wb,  gamma_model,  sheet = "gamma_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_model")
    default_beta    <- diag(nhop) * 0
    beta_model      <- matrix(default_beta, ncol = nhop,
                              dimnames = list(paste("HoP_", 1:nhop, sep = ""),  paste("HoP_", 1:nhop, sep = "")))
    openxlsx::writeData(wb,  beta_model,  sheet = "beta_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "epsilon_initialvalues")
    default_muep_initial   <- rep(.1, ncovariates)
    default_musig_initial  <- rep(NA, ncovariates)
    epsilon_model_initial  <- matrix(cbind(default_muep_initial, default_musig_initial),  ncol = 2,
                                     dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  epsilon_model_initial,  sheet = "epsilon_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_initialvalues")
    default_deltaep_initial   <- rep(NA, nhop)
    default_deltasig_initial  <- rep(.1, nhop)
    delta_model_initial       <- matrix(cbind(default_deltaep_initial, default_deltasig_initial),  ncol = 2,
                                        dimnames = list(paste("HoP_", 1:nhop, sep = ""), c("mu", "sigma")))
    openxlsx::writeData(wb,  delta_model_initial,  sheet = "delta_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_initialvalues")
    default_gamma_initial   <- diag(nhop) * NA
    gamma_model_initial     <- matrix(default_gamma_initial,  ncol = nhop,
                                      dimnames = list(attribute_names, paste("HoP_", 1:nhop, sep = "")))
    openxlsx::writeData(wb,  gamma_model_initial,  sheet = "gamma_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_initialvalues")
    default_beta_initial   <- diag(nhop) * NA
    beta_model_initial      <- matrix(default_beta_initial,  ncol = nhop,
                                      dimnames = list(paste("HoP_", 1:nhop, sep = ""), paste("HoP_", 1:nhop, sep = "")))
    openxlsx::writeData(wb,  beta_model_initial,  sheet = "beta_initialvalues",  rowNames = TRUE)

  }else if (model_type == "one-factor") {

    openxlsx::addWorksheet(wb,  "epsilon_model")
    default_muep  <- rep(1, ncovariates)
    default_musig <- rep(0, ncovariates)
    epsilon_model <- matrix(cbind(default_muep, default_musig), ncol = 2,
                            dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "epsilon_model",  epsilon_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_model")
    nhop <- 1
    default_deltaep   <- rep(0, nhop)
    default_deltasig  <- rep(-1, nhop)
    delta_model       <- matrix(cbind(default_deltaep, default_deltasig),  ncol = 2,
                                dimnames = list("HoP_1", c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "delta_model",  delta_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_model")
    default_gamma     <- rep(1, ncovariates)
    gamma_model       <- matrix(default_gamma,  ncol = 1,
                                dimnames = list(attribute_names, "HoP_1"))
    openxlsx::writeData(wb,  gamma_model,  sheet = "gamma_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_model")
    default_beta      <- 0
    beta_model        <- matrix(default_beta,  ncol = 1,
                                dimnames = list("HoP_1", "HoP_1"))
    openxlsx::writeData(wb,  beta_model,  sheet = "beta_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "epsilon_initialvalues")
    default_muep_initial   <- rep(.1, ncovariates)
    default_musig_initial  <- rep(NA, ncovariates)
    epsilon_model_initial  <- matrix(cbind(default_muep_initial, default_musig_initial),
                                     ncol = 2,  dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  epsilon_model_initial,  sheet = "epsilon_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_initialvalues")
    default_deltaep_initial   <- rep(NA, 1)
    default_deltasig_initial  <- rep(NA, 1)
    delta_model_initial       <- matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                        ncol = 2,  dimnames = list("HoP_1", c("mu", "sigma")))
    openxlsx::writeData(wb,  delta_model_initial,  sheet = "delta_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_initialvalues")
    default_gamma_initial   <- rep(.1, ncovariates)
    gamma_model_initial     <- matrix(default_gamma_initial,  ncol = 1,
                                      dimnames = list(attribute_names, "HoP_1"))
    openxlsx::writeData(wb,  gamma_model_initial,  sheet = "gamma_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_initialvalues")
    default_beta_initial   <- rep(NA, 1)
    beta_model_initial      <- matrix(default_beta_initial,  ncol = 1,
                                      dimnames = list("HoP_1", "HoP_1"))
    openxlsx::writeData(wb,  beta_model_initial,  sheet = "beta_initialvalues",  rowNames = TRUE)

  }else if (model_type == "mtmm") {

    openxlsx::addWorksheet(wb,  "epsilon_model")
    default_muep  <- rep(1, ncovariates)
    default_musig <- rep(0, ncovariates)
    epsilon_model <- matrix(cbind(default_muep, default_musig),  ncol = 2,
                            dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "epsilon_model",  epsilon_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_model")
    nhop <- ncovariates
    default_deltaep   <- rep(0, nhop / 2 + 2)
    default_deltasig  <- rep(-1, nhop / 2 + 2)
    delta_model <- matrix(cbind(default_deltaep, default_deltasig),  ncol = 2,
                          dimnames = list(paste("HoP_", 1:(nhop / 2 + 2), sep = ""), c("mu", "sigma")))
    openxlsx::writeData(wb,  sheet = "delta_model",  delta_model,  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_model")
    default_gamma   <- rbind(cbind(diag(ncovariates / 2) * 1,
                                   matrix(cbind(rep(1, ncovariates / 2), rep(0, ncovariates / 2)), ncol = 2)),
                             cbind(diag(ncovariates / 2) * 1,
                                   matrix(cbind(rep(0, ncovariates / 2), rep(1, ncovariates / 2)), ncol = 2)))
    gamma_model <- matrix(default_gamma,  ncol = (ncovariates / 2) + 2,
                          dimnames = list(attribute_names, paste("HoP_", 1:(ncovariates / 2 + 2), sep = "")))
    openxlsx::writeData(wb,  gamma_model,  sheet = "gamma_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_model")
    default_beta   <- diag(nhop / 2 + 2) * 0
    beta_model <- matrix(default_beta, ncol = (nhop / 2 + 2),
                         dimnames = list(paste("HoP_", 1:(nhop / 2 + 2), sep = ""),
                                         paste("HoP_", 1:(nhop / 2 + 2), sep = "")))
    openxlsx::writeData(wb,  beta_model,  sheet = "beta_model",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "epsilon_initialvalues")
    default_muep_initial   <- rep(.1, ncovariates)
    default_musig_initial   <- rep(NA, ncovariates)
    epsilon_model_initial  <- matrix(cbind(default_muep_initial, default_musig_initial),
                                     ncol = 2, dimnames = list(attribute_names, c("mu", "sigma")))
    openxlsx::writeData(wb,  epsilon_model_initial,  sheet = "epsilon_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "delta_initialvalues")
    default_deltaep_initial   <- rep(NA, nhop)
    default_deltasig_initial  <- rep(NA, nhop)
    delta_model_initial <- matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                  ncol = 2,  dimnames = list(paste("HoP_", 1:nhop, sep = ""), c("mu", "sigma")))
    openxlsx::writeData(wb,  delta_model_initial,  sheet = "delta_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "gamma_initialvalues")
    default_gamma_initial   <- rbind(cbind(diag(ncovariates / 2) * .1,
                                           matrix(cbind(rep(.1, ncovariates / 2), rep(NA, ncovariates / 2)), ncol = 2)),
                                     cbind(diag(ncovariates / 2) * .1,
                                           matrix(cbind(rep(NA, ncovariates / 2), rep(.1, ncovariates / 2)), ncol = 2))
    )
    gamma_model_initial <- matrix(default_gamma_initial,
                                  ncol = (ncovariates / 2) + 2,
                                  dimnames = list(attribute_names,
                                                  paste("HoP_", 1:(ncovariates / 2 + 2), sep = "")
                                  )
    )
    gamma_model_initial[gamma_model_initial == 0] <- NA
    openxlsx::writeData(wb,  gamma_model_initial,  sheet = "gamma_initialvalues",  rowNames = TRUE)

    openxlsx::addWorksheet(wb,  "beta_initialvalues")
    default_beta_initial   <- diag(nhop / 2 + 2) * NA
    beta_model_initial  <- matrix(default_beta_initial, ncol = nhop / 2 + 2,
                                  dimnames = list(paste("HoP_", 1:(nhop / 2 + 2), sep = ""),
                                                  paste("HoP_", 1:(nhop / 2 + 2), sep = "")))
    openxlsx::writeData(wb,  beta_model_initial,  sheet = "beta_initialvalues",  rowNames = TRUE)
  }

  openxlsx::saveWorkbook(wb,  file = file_name,  overwrite = TRUE)

  if (verbose > 0) {
    message(paste("\n", model_type,  "EMI model file saved as",  file_name, sep = ""))
  }

}
