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
  }

  if (header == FALSE){
    names(data) <- c("ID",
                     "group",
                     "choice",
                     paste0("V", 1:(ncol(data)-3)))


  }

  return(data)
}

#' Read data. This documentation is incomplete.
#'
#' @param data input
#' @param header deafult TRUE
#' @returns A list of the processed data.
#' @export
setUp <- function(data, header = TRUE) {


  if (inherits(data,"character")) {
    #read in data
    data_matrix <- readData(data, header = header)
    filename  <-  data
  }else if (inherits(data, c("data.frame", "tibble", "matrix"))) {
    data_matrix <-  as.data.frame(data)
    filename  <-  NULL
  }else {
    stop("Data must either be a filename or data as a matrix, tibble, or data.frame")
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

  return(processed)

}

#' remove variables This documentation is incomplete.
#' @param processed_data processes data list
#' @param variable variable to remove, string or number or vector
#' @param verbose 1 means print more
#' @returns a new processed data object with everything updated
#' @export
remove_variables <- function(processed_data, variable, verbose=0) {
  data <- processed_data$data_original

  if(class(variable) != "character" & class(variable) != "numeric"){
    stop("Variables must either be named or selected by column number.")
  }

  if(class(variable) == "numeric"){

    if(any(variable<=3)){
      stop("Cannot remove first three columns.")
    }

    if(any(!variable %in% (4:ncol(data)))){
      stop("Columns not in range.")
    }

    if(verbose>0){
      message(paste0("Removing variables: ",  paste(names(data)[c(variable)], collapse = ", ")))
    }

    data <- data[, -c(variable)]
  }


  if(class(variable) == "character"){

    idx <- which(names(data) %in% c(variable))

    if(length(idx) != length(variable)){
      stop("Columns not found.")
    }

    if(any(idx<=3)){
      stop("Cannot remove first three columns.")
    }

    if(verbose>0){
      message(paste0("Removing variables: ",  paste(names(data)[c(idx)], collapse = ", ")))
    }

    data <- data[, !names(data) %in% c(variable)]
  }

  #we actually have to regenerate each time because concepts matrix will change.
  return(setUp(data))
}




#' select variables This documentation is incomplete.
#' @param processed_data processes data list
#' @param variable variable to remove, string or number or vector
#' @param verbose 1 means print more, 0 no print, default 1
#' @returns a new processed data object with everything updated
#' @export
select_variables <- function(processed_data, variable, verbose=1) {
  data <- processed_data$data_original

  if(class(variable) != "character" & class(variable) != "numeric"){
    stop("Variables must either be named or selected by column number.")
  }

  if(class(variable) == "numeric"){

    if(any(variable<=3)){
      stop("Cannot select first three columns - they are always included.")
    }

    if(any(!variable %in% (4:ncol(data)))){
      stop("Selected columns not in range.")
    }

    if(verbose>0){
      message(paste0("Selecting variables: ",  paste(names(data)[c(variable)], collapse = ", ")))
    }

    data <- cbind(data[,1:3], data[, c(variable)])
  }


  if(class(variable) == "character"){

    idx <- which(names(data) %in% c(variable))

    if(length(idx) != length(variable)){
      stop("Selected columns not found.")
    }

    if(any(idx<=3)){
      stop("Cannot select first three columns - they are always included.")
    }

    if(verbose>0){
      message(paste0("Selecting variables: ",  paste(names(data)[c(idx)], collapse = ", ")))
    }

    data <- cbind(data[,1:3], data[, names(data) %in% c(variable)])
  }

  #we actually have to regenerate each time because concepts matrix will change.
  return(setUp(data))
}



