#' Read data. This documentation is incomplete.
#' @param filename filename string
#' @param header deafult TRUE
#' @returns A dataframe of the input data.
#' @export
readData  <-  function(filename, header = TRUE) {

  #checks file type and then reads accordingly
  if (tools::file_ext(filename) == "txt") {
    data <- read.table(filename, header = header)
  }else if (tools::file_ext(filename) == "csv") {
    data <- read.csv(filename, header = header)
  }else if (tools::file_ext(filename) == "xls" || file_ext(filename) == "xlsx") {
    data <- as.data.frame(readxl::read_excel(filename, sheet = 1, col_names = header))
  }else if (tools::file_ext(filename) == "tsv") {
    data <- read.table(filename, sep = "\t", header = header)
  }else {
    cli::cli_abort("File type not supported. Currently supported formats are: csv, xls, xlsx, tsv.")
  }

  if (header == FALSE) {
    names(data) <- c("ID",
                     "group",
                     "choice",
                     paste0("V", 1:(ncol(data) - 3)))


  } else {
    names(data)[1:3] <- c("ID", "group", "choice") #keep first 3 column names consistent
  }

  #some later methods with fail if data is not sorted by the ID column
  data <- data[order(data[, 1]), ]

  return(data)
}

#' Read data. This documentation is incomplete.
#'
#' @param data input
#' @param header deafult TRUE
#' @param verbose default FALSE
#' @returns A list of the processed data.
#' @export
setUp <- function(data, header = TRUE, verbose = FALSE) {


  if (inherits(data, "character")) {
    #read in data
    data_matrix <- readData(data, header = header)
    filename  <-  data

    if (verbose) {
      cli::cli_inform("Data read in from {data}.")
    }
  }else if (inherits(data, c("data.frame", "tibble", "matrix"))) {
    data_matrix <-  as.data.frame(data)
    filename  <-  NULL
  }else {
    cli::cli_abort("Data must either be a filename or data as a matrix, tibble, or data.frame")
  }

  #from Kobe code
  nmax_choiceset_size <- as.numeric(max(unlist(rle(data_matrix[, 2])[1])))

  #concept list
  concept_list <- createConcepts(data_matrix, nmax_choiceset_size)

  #fdd
  fdd <- frequencyDistribution(concept_list)

  #some more intermediate processing
  ndecisionmakers <- dim(fdd)[1]

  lcovariates <- array(0, concept_list$ncovariates)

  for (i in 1:concept_list$ncovariates) {
    lcovariates[i] <- paste("Cov", i)
  }


  #all the initial stuff packaged up
  processed <- list(data_original = data_matrix,
                    data_name = filename,
                    data = concept_list$data,
                    ncovariates = concept_list$ncovariates,
                    npp = concept_list$ncovariates,
                    nmax_choiceset_size = nmax_choiceset_size,
                    ndecisionmakers = ndecisionmakers,
                    concept = concept_list$concept,
                    lcovariates = lcovariates,
                    fdd = fdd,
                    attribute_names  =  names(data_matrix)[-(1:3)]
  )

  if (verbose) {
    cli::cli_inform("Data successfully processed.")
  }

  return(processed)

}

#' remove variables This documentation is incomplete.
#' @param processed_data processes data list
#' @param variable variable to remove, string or number or vector
#' @param verbose TRUE means print more
#' @returns a new processed data object with everything updated
#' @export
removeVariables <- function(processed_data, variable, verbose = FALSE) {
  data <- processed_data$data_original

  if (any(is.na(data))) {
    cli::cli_abort("processed_data$data_original is NA.
                   selectVariables and removeVariables must use prior to joinChoiceDatasets.")
  }

  if (!inherits(variable, "character") && !inherits(variable, "numeric")) {
    cli::cli_abort("Variables must either be named or selected by column number.")
  }

  if (inherits(variable, "numeric")) {

    if (any(variable <= 3)) {
      cli::cli_abort("Cannot remove first three columns.")
    }

    if (any(!variable %in% (4:ncol(data)))) {
      cli::cli_abort("Columns not in range.")
    }

    if (verbose) {
      cli::cli_inform(paste0("Removing variables: ",  paste(names(data)[c(variable)], collapse = ", ")))
    }

    data <- data[, -c(variable)]
  }


  if (inherits(variable, "character")) {

    idx <- which(names(data) %in% c(variable))

    if (length(idx) != length(variable)) {
      cli::cli_abort("Columns not found.")
    }

    if (any(idx <= 3)) {
      cli::cli_abort("Cannot remove first three columns.")
    }

    if (verbose) {
      cli::cli_inform(paste0("Removing variables: ",  paste(names(data)[c(idx)], collapse = ", ")))
    }

    data <- data[, !names(data) %in% c(variable)]
  }

  #we actually have to regenerate each time because concepts matrix will change.
  return(setUp(data))
}




#' select variables This documentation is incomplete.
#' @param processed_data processes data list
#' @param variable variable to remove, string or number or vector
#' @param verbose TRUE
#' @returns a new processed data object with everything updated
#' @export
selectVariables <- function(processed_data, variable, verbose = FALSE) {

  data <- processed_data$data_original

  if (any(is.na(data))) {
    cli::cli_abort("processed_data$data_original is NA.
                   selectVariables and removeVariables must use prior to joinChoiceDatasets.")
  }



  if (!inherits(variable, "character") && !inherits(variable, "numeric")) {
    cli::cli_abort("Variables must either be named or selected by column number.")
  }

  if (inherits(variable, "numeric")) {

    if (any(variable <= 3)) {
      cli::cli_abort("Cannot select first three columns - they are always included.")
    }

    if (any(!variable %in% (4:ncol(data)))) {
      cli::cli_abort("Selected columns not in range.")
    }

    if (verbose) {
      cli::cli_inform(paste0("Selecting variables: ",  paste(names(data)[c(variable)], collapse = ", ")))
    }

    data <- cbind(data[, 1:3], data[, c(variable)])
  }


  if (inherits(variable, "character")) {

    idx <- which(names(data) %in% c(variable))

    if (length(idx) != length(variable)) {
      cli::cli_abort("Selected columns not found.")
    }

    if (any(idx <= 3)) {
      cli::cli_abort("Cannot select first three columns - they are always included.")
    }

    if (verbose) {
      cli::cli_inform(paste0("Selecting variables: ",  paste(names(data)[c(idx)], collapse = ", ")))
    }

    data <- cbind(data[, 1:3], data[, names(data) %in% c(variable)])
  }

  #we actually have to regenerate each time because concepts matrix will change.
  return(setUp(data))
}


#' Frequency distribution
#' this probably shouldnt be exported to be honest this whole file needs refactoring
#' @param cs data
#' @returns fdd
#' @export
frequencyDistribution <- function(cs) {

  coldd <- cs$data[, 1]
  cold <- matrix(coldd, length(coldd), 1)

  fdd <- frequencyDistributionCpp(cold)

  return(fdd)
}
