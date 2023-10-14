#' generate_model_matrices
#'
#' @param pre_processed_data processed data list
#' @param model_type string
#' @returns model matrices
#' @export
generate_model_matrices  <-  function(pre_processed_data,  model_type)  {

  #setup values
  ncovariates  <-  pre_processed_data$ncovariates
  attribute_names  <-  pre_processed_data$attribute_names

  if (model_type == "fixed") {

    nhop <- NA

    default_muep  <-  rep(1, ncovariates)
    default_musig  <-  rep(0, ncovariates)
    default_deltaep  <-  0
    default_deltasig  <-  -1
    default_gamma  <-  rep(0, ncovariates)
    default_beta  <-  0

    default_muep_initial  <-  rep(.1, ncovariates)
    default_musig_initial  <-  rep(NA, ncovariates)
    default_deltaep_initial  <-  rep(NA, 1)
    default_deltasig_initial  <-  rep(NA, 1)
    default_gamma_initial  <-  rep(NA, ncovariates)
    default_beta_initial  <-  rep(NA, 1)

    epsilon_model  <-  matrix(cbind(default_muep, default_musig),
                              ncol = 2,
                              dimnames = list(attribute_names, c("mu", "sigma"))
    )

    delta_model  <-  matrix(cbind(default_deltaep, default_deltasig),
                            ncol = 2,
                            dimnames = list("HoP_1", c("mu", "sigma"))
    )

    gamma_model  <-  matrix(default_gamma,
                            ncol = 1,
                            dimnames = list(attribute_names, "HoP_1")
    )

    beta_model  <-  matrix(default_beta,
                           ncol = 1,
                           dimnames = list("HoP_1", "HoP_1")
    )

    epsilon_model_initial  <-  matrix(cbind(default_muep_initial, default_musig_initial),
                                      ncol = 2,
                                      dimnames = list(attribute_names, c("mu", "sigma"))
    )

    delta_model_initial  <-  matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                    ncol = 2,
                                    dimnames = list("HoP_1", c("mu", "sigma"))
    )

    gamma_model_initial  <-  matrix(default_gamma_initial,
                                    ncol = 1,
                                    dimnames = list(attribute_names, "HoP_1")
    )

    beta_model_initial  <-  matrix(default_beta_initial,
                                   ncol = 1,
                                   dimnames = list("HoP_1", "HoP_1")
    )

  }else if (model_type == "random") {

    nhop  <-  ncovariates

    default_muep  <-  rep(1, ncovariates)
    default_musig  <-  rep(0, ncovariates)
    default_deltaep  <-  rep(0, nhop)
    default_deltasig  <-  rep(1, nhop)
    default_gamma  <-  diag(nhop)  *  -1
    default_beta  <-  diag(nhop)  *  0

    default_muep_initial  <-  rep(.1, ncovariates)
    default_musig_initial  <-  rep(NA, ncovariates)
    default_deltaep_initial  <-  rep(NA, nhop)
    default_deltasig_initial  <-  rep(.1, nhop)
    default_gamma_initial  <-  diag(nhop)  *  NA
    default_beta_initial  <-  diag(nhop)  *  NA

    epsilon_model  <-  matrix(cbind(default_muep, default_musig),
                              ncol = 2,
                              dimnames = list(attribute_names,  c("mu", "sigma"))
    )

    delta_model  <-  matrix(cbind(default_deltaep, default_deltasig),
                            ncol = 2,
                            dimnames = list(paste("HoP_", 1:nhop, sep = ""),  c("mu", "sigma"))
    )

    gamma_model  <-  matrix(default_gamma,
                            ncol = nhop,
                            dimnames = list(attribute_names,  paste("HoP_", 1:nhop, sep = ""))
    )

    beta_model  <-  matrix(default_beta,
                           ncol = nhop,
                           dimnames = list(paste("HoP_", 1:nhop, sep = ""),
                                           paste("HoP_", 1:nhop, sep = ""))
    )

    epsilon_model_initial  <-  matrix(cbind(default_muep_initial, default_musig_initial),
                                      ncol = 2,
                                      dimnames = list(attribute_names, c("mu", "sigma"))
    )

    delta_model_initial  <-  matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                    ncol = 2,
                                    dimnames = list(paste("HoP_", 1:nhop, sep = ""),
                                                    c("mu", "sigma"))
    )

    gamma_model_initial  <-  matrix(default_gamma_initial,
                                    ncol = nhop,
                                    dimnames = list(attribute_names,
                                                    paste("HoP_", 1:nhop, sep = ""))
    )

    beta_model_initial  <-  matrix(default_beta_initial,
                                   ncol = nhop,
                                   dimnames = list(paste("HoP_", 1:nhop, sep = ""),
                                                   paste("HoP_", 1:nhop, sep = ""))
    )

  }else if (model_type == "one-factor") {

    nhop <- 1

    default_muep  <-  rep(1, ncovariates)
    default_musig  <-  rep(0, ncovariates)
    default_deltaep  <-  rep(0, nhop)
    default_deltasig  <-  rep(-1, nhop)
    default_gamma  <-  rep(1, ncovariates)
    default_beta  <-  0

    default_muep_initial  <-  rep(.1, ncovariates)
    default_musig_initial  <-  rep(NA, ncovariates)
    default_deltaep_initial  <-  rep(NA, 1)
    default_deltasig_initial  <-  rep(NA, 1)
    default_gamma_initial  <-  rep(.1, ncovariates)
    default_beta_initial  <-  rep(NA, 1)

    epsilon_model  <-  matrix(cbind(default_muep, default_musig),
                              ncol = 2,
                              dimnames = list(attribute_names, c("mu", "sigma"))
    )

    delta_model  <-  matrix(cbind(default_deltaep, default_deltasig),
                            ncol = 2,
                            dimnames = list("HoP_1", c("mu", "sigma"))
    )

    gamma_model  <-  matrix(default_gamma,
                            ncol = 1,
                            dimnames = list(attribute_names, "HoP_1")
    )

    beta_model <-  matrix(default_beta,
                          ncol = 1,
                          dimnames = list("HoP_1", "HoP_1")
    )

    epsilon_model_initial  <-  matrix(cbind(default_muep_initial, default_musig_initial),
                                      ncol = 2,
                                      dimnames = list(attribute_names, c("mu", "sigma"))
    )

    delta_model_initial  <-  matrix(cbind(default_deltaep_initial, default_deltasig_initial),
                                    ncol = 2,
                                    dimnames = list("HoP_1", c("mu", "sigma"))
    )

    gamma_model_initial  <-  matrix(default_gamma_initial,
                                    ncol = 1,
                                    dimnames = list(attribute_names, "HoP_1")
    )

    beta_model_initial  <-  matrix(default_beta_initial,
                                   ncol = 1,
                                   dimnames = list("HoP_1", "HoP_1")
    )



  }

  model_matrices  <- list(
    epsilon_model = epsilon_model,
    delta_model = delta_model,
    gamma_model = gamma_model,
    beta_model = beta_model,
    epsilon_model_initial = epsilon_model_initial,
    delta_model_initial = delta_model_initial,
    gamma_model_initial = gamma_model_initial,
    beta_model_initial = beta_model_initial
  )

  return(model_matrices)

}



#' generate model
#' @param pre_processed_data processed data list
#' @param model_type string
#' @param file_name only for emi
#' @param matrix_list only for manual
#' @returns model
#' @export
model_generator  <-  function(pre_processed_data, model_type, file_name = NULL, matrix_list = NULL) {

  model_types  <-  c("fixed",  "random",  "one-factor", "manual", "emi", "mtmm")

  if (!(model_type %in% model_types)) {
    stop(paste("model_type not one of ",  paste(model_types,  collapse = ",  ")))
  }

  if (model_type %in% c("fixed",  "random",  "one-factor")) {

    matrix_list <- generate_model_matrices(pre_processed_data,  model_type)
    description  <-  paste0("Generated model of type ", model_type)

    nhop <- nrow(matrix_list$delta_model)
    npp <- pre_processed_data$npp
    ncovariates <- pre_processed_data$ncovariates

    initial_e  <-   matrix_list$epsilon_model_initial
    initial_e  <-   na.omit(as.vector(initial_e))
    initial_d  <-   matrix_list$delta_model_initial
    initial_d  <-   na.omit(as.vector(initial_d))
    initial_g  <-   matrix_list$gamma_model_initial
    initial_g  <-   na.omit(as.vector(initial_g))
    initial_b  <-   matrix_list$beta_model_initial
    initial_b  <-   na.omit(as.vector(initial_b))
    initial_values  <-  c(initial_e, initial_d, initial_g, initial_b)

    phi  <-  diag(npp + nhop)

    code  <-  matrix(0, ncovariates, npp)
    if (ncovariates == npp) {
      diag(code) <- 1
    }

  }else if (model_type == "manual") {
    if (is.null(matrix_list)) {
      stop("Need to supply matrix_list for manual model design.")
    }

    name_checks <- c(
      is.null(matrix_list[["epsilon_model"]]),
      is.null(matrix_list[["delta_model"]]),
      is.null(matrix_list[["gamma_model"]]),
      is.null(matrix_list[["beta_model"]]),
      is.null(matrix_list[["epsilon_model_initial"]]),
      is.null(matrix_list[["delta_model_initial"]]),
      is.null(matrix_list[["gamma_model_initial"]]),
      is.null(matrix_list[["beta_model_initial"]])
    )

    if (any(name_checks)) {
      stop("Matrix list does not contain all required model elements.")
    }

    description  <-  paste0("Manually entered model")
    description  <-  paste0("Generated model of type ", model_type)

    nhop <- nrow(matrix_list$delta_model)
    npp <- pre_processed_data$npp
    ncovariates <- pre_processed_data$ncovariates

    initial_e  <-   matrix_list$epsilon_model_initial
    initial_e  <-   na.omit(as.vector(initial_e))
    initial_d  <-   matrix_list$delta_model_initial
    initial_d  <-   na.omit(as.vector(initial_d))
    initial_g  <-   matrix_list$gamma_model_initial
    initial_g  <-   na.omit(as.vector(initial_g))
    initial_b  <-   matrix_list$beta_model_initial
    initial_b  <-   na.omit(as.vector(initial_b))
    initial_values  <-  c(initial_e, initial_d, initial_g, initial_b)

    phi  <-  diag(npp + nhop)

    code  <-  matrix(0, ncovariates, npp)
    if (ncovariates == npp) {
      diag(code) <- 1
    }

  }else if (model_type == "emi") {
    if (is.null(file_name)) {
      stop(paste("Need to supply file_name for EMI model design."))
    }
    description  <-  paste0("EMI model via spreadsheet.")

    npp <- pre_processed_data$npp
    ncovariates <- pre_processed_data$ncovariates

    EMI <- openxlsx::loadWorkbook(file = file_name, isUnzipped = FALSE)
    ncovariates_emi <- nrow(openxlsx::read.xlsx(EMI, sheet = 1))
    npp_emi  <- ncovariates
    nhop_emi <- nrow(openxlsx::read.xlsx(EMI, sheet = 2))
    nhop <- nhop_emi

    if (npp_emi != npp) {
      stop(paste("npp from EMI does not agree with npp from data"))
    }

    if (ncovariates_emi != ncovariates) {
      stop(paste("ncovariates from EMI does not agree with npp from data"))
    } #dont need this for nhop

    code     <- matrix(c(1:ncovariates * npp) * 0, ncovariates, npp)
    epsilon  <- as.matrix(openxlsx::read.xlsx(EMI, sheet = 1, cols = c(2, 3)))
    delta    <- as.matrix(openxlsx::read.xlsx(EMI, sheet = 2, cols = c(2, 3)))
    gamma    <- as.matrix(openxlsx::read.xlsx(EMI, sheet = 3, cols = c(2:(1 + nhop))))
    beta     <- as.matrix(openxlsx::read.xlsx(EMI, sheet = 4, cols = c(2:(1 + nhop))))
    phi      <- matrix(c(1:(npp + nhop)) * 0, npp + nhop, npp + nhop)

    for (i in 1:(npp + nhop)){
      phi[i, i] <- 1
    }

    if (ncovariates == npp_emi) {
      for (i1 in 1:npp){
        code[i, i] <- 1
      }
    }

    initial_e <-  as.matrix(openxlsx::read.xlsx(EMI, sheet = 5, cols = c(2, 3)))
    initial_e <-  na.omit(as.vector(initial_e))

    initial_d <-  as.matrix(openxlsx::read.xlsx(EMI, sheet = 6, cols = c(2, 3)))
    initial_d <-  na.omit(as.vector(initial_d))

    initial_g <-  as.matrix(openxlsx::read.xlsx(EMI, sheet = 7, cols = c(2:(1 + nhop))))
    initial_g <-  na.omit(as.vector(initial_g))

    initial_b <-  as.matrix(openxlsx::read.xlsx(EMI, sheet = 8, cols = c(2:(1 + nhop))))
    initial_b <-  na.omit(as.vector(initial_b))

    initial_values <- c(initial_e, initial_d, initial_g, initial_b)

    matrix_list[["epsilon_model"]] <- epsilon
    matrix_list[["delta_model"]] <- delta
    matrix_list[["gamma_model"]] <- gamma
    matrix_list[["beta_model"]] <- beta


  }else if (model_type == "mtmm") {
    #FIX
  }

  model <- list(description = description,
                data = pre_processed_data,
                ncovariates = ncovariates,
                npp = npp,
                nhop = nhop,
                code = code,
                epsilon = matrix_list$epsilon_model,
                delta = matrix_list$delta_model,
                gamma = matrix_list$gamma_model,
                beta = matrix_list$beta_model,
                phi = phi,
                initial_values = initial_values)

  return(model)
}
