#' Create workbook
#'
#' @returns name of workbook
#' @export
createEMIWorkbook <- function(pre_processed_data,  model_type,  working_folder = NULL) {

  model_types <- c("fixed",  "random",  "one-factor",  "mtmm")

  if (!(model_type %in% model_types)) {
    stop(paste("model_type not one of ",  paste(model_types,  collapse = ",  ")))
  }

  if (is.null(working_folder)) {
    working_folder <- getwd() #maybe dont use this
  }

  #create workbook
  emi_name <- paste0(working_folder,  "/EMIs/EMI_",  model_type, ".xlsx")
  wb <- openxlsx::createWorkbook(emi_name)

  matrix_list <- generate_model_matrices(pre_processed_data,  model_type)

  #Write all sheets

  openxlsx::addWorksheet(wb,  "epsilon_model")
  openxlsx::writeData(wb,
                      matrix_list$epsilon_model,
                      sheet = "epsilon_model",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "delta_model")
  openxlsx::writeData(wb,
                      matrix_list$delta_model,
                      sheet = "delta_model",
                      rowNames = TRUE
                      )


  openxlsx::addWorksheet(wb,  "gamma_model")
  openxlsx::writeData(wb,
                      matrix_list$gamma_model,
                      sheet = "gamma_model",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "beta_model")
  openxlsx::writeData(wb,
                      matrix_list$beta_model,
                      sheet = "beta_model",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "epsilon_initial_values")
  openxlsx::writeData(wb,
                      matrix_list$epsilon_model_initial,
                      sheet = "epsilon_initial_values",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "delta_initial_values")
  openxlsx::writeData(wb,
                      matrix_list$delta_model_initial,
                      sheet = "delta_initial_values",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "gamma_initial_values")
  openxlsx::writeData(wb,
                      matrix_list$gamma_model_initial,
                      sheet = "gamma_initial_values",
                      rowNames = TRUE
                      )

  openxlsx::addWorksheet(wb,  "beta_initial_values")
  openxlsx::writeData(wb,
                      matrix_list$beta_model_initial,
                      sheet = "beta_initial_values",
                      rowNames = TRUE
                      )

  #save workbook
  openxlsx::saveWorkbook(wb,  file = emi_name,  overwrite = TRUE)
  message(paste0("\n", "EMI model file ",  emi_name, " saved."))

  return(emi_name)
}

#this needs to be edited so the EMI model is joined to an existing data set
#' Loads an existing workbook
#'
#' @returns model
#' @export
loadEMIWorkbook <- function(emi_file_name) {

  EMI <- openxlsx::loadWorkbook(emi_file_name, isUnzipped = FALSE)

  ncovariates <- nrow(openxlsx::read.xlsx(EMI, sheet = 1))
  npp <- ncovariates
  nhop <- nrow(openxlsx::read.xlsx(EMI,  sheet = 2))

  code <- matrix(0, ncovariates, npp)
  if (ncovariates == npp) {
    diag(code) <- 1
  }

  epsilon <- as.matrix(openxlsx::read.xlsx(EMI,  sheet = 1,  cols = c(2, 3)))
  delta <- as.matrix(openxlsx::read.xlsx(EMI,  sheet = 2,  cols = c(2, 3)))
  gamma <- as.matrix(openxlsx::read.xlsx(EMI,  sheet = 3,  cols = c(2:(1 + nhop))))
  beta <- as.matrix(openxlsx::read.xlsx(EMI,  sheet = 4,  cols = c(2:(1 + nhop))))

  phi <- diag(npp + nhop)

  initial_e <-  as.matrix(openxlsx::read.xlsx(EMI,  sheet = 5,  cols = c(2, 3)))
  initial_e <-  na.omit(as.vector(initial_e))
  initial_d <-  as.matrix(openxlsx::read.xlsx(EMI,  sheet = 6,  cols = c(2, 3)))
  initial_d <-  na.omit(as.vector(initial_d))
  initial_g <-  as.matrix(openxlsx::read.xlsx(EMI,  sheet = 7,  cols = c(2:(1 + nhop))))
  initial_g <-  na.omit(as.vector(initial_g))
  initial_b <-  as.matrix(openxlsx::read.xlsx(EMI,  sheet = 8,  cols = c(2:(1 + nhop))))
  initial_b <-  na.omit(as.vector(initial_b))

  initial_values <- c(initial_e, initial_d, initial_g, initial_b)

  description <- "" #need something here

  model <- list(description = description,
              data = NA, #pre_processed_data,
              ncovariates = ncovariates,
              npp = npp,
              nhop = nhop,
              code = code,
              epsilon = epsilon,
              delta = delta,
              gamma = gamma,
              beta = beta,
              phi = phi,
              initial_values = initial_values)

  return(model)

}
