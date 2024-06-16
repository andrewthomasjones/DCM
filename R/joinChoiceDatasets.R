#' join
#' @param data1 processed data list
#' @param data2 processed data list
#' @returns joins two processed objects by cols. only takes  ID ChoiceSet Choice from data1
#' variable names must be in format variable_dataset and match across the two datasets
#' @export
joinChoiceDatasets  <-  function(data1,  data2) {

  #a lot of checks because a lot can go wrong here

  if (length(data1$attribute_names) != length(data2$attribute_names)) {
    cli::cli_abort("Differing numbers of variables between first and second dataset.")
  }

  data1_names <- stringr::str_match(data1$attribute_names, "^([[:alnum:]]{1,})_([[:alnum:]]{1,})$")
  data2_names <- stringr::str_match(data2$attribute_names, "^([[:alnum:]]{1,})_([[:alnum:]]{1,})$")

  #removed so first dataset can have multiple so this can be done for more than just 2
  # if (length(unique(data1_names[, 3])) != 1) {
  #   cli::cli_abort("Variable Name Suffixes in first dataset are not consistent.")
  # }

  if (length(unique(data2_names[, 3])) != 1) {
    cli::cli_abort("Variable Name Suffixes in second dataset are not consistent.")
  }

  if (length(setdiff(data1_names[, 2], data2_names[, 2]))) {
    cli::cli_abort("Variable names are not consistent between first and second dataset.")
  }

  # if (!all(data1_names[, 2] == data2_names[, 2])) {
  #   cli::cli_abort("Variables are not in the same order in both datasets.")
  # }

  nconcepts_data1 <- dim(data1$concept)[1]
  nconcepts_data2 <- dim(data2$concept)[1]

  ncovariates_data1 <- data1$ncovariates
  ncovariates_data2 <- data2$ncovariates

  nrows_data1 <- dim(data1$data)[1]
  nrows_data2 <- dim(data2$data)[1]
  ncols_data1 <- dim(data1$data)[2]
  ncols_data2 <- dim(data2$data)[2]


  #data_original_merge <- data.frame(data1$data_original, data2$data_original[, 4:ncol(data2$data_original)])

  nmaxchoicesetsize_merge <- max(data1$nmax_choiceset_size, data2$nmax_choiceset_size)

  nrows_concept_merge <- nconcepts_data1 + nconcepts_data2
  ncols_concept_merge <- ncovariates_data1 + ncovariates_data2

  concept_merge <- matrix(0, nrow = nrows_concept_merge, ncol = ncols_concept_merge)

  concept_merge[1:nconcepts_data1, 1:ncovariates_data1] <- data1$concept
  concept_merge[(nconcepts_data1 + 1):nrows_concept_merge, (ncovariates_data1 + 1):ncols_concept_merge] <- data2$concept

  nrows_data_merge <- nrows_data1 + nrows_data2
  ncols_data_merge <- max(ncols_data1, ncols_data2)

  data_merge  <- matrix(0, nrow = nrows_data_merge, ncol = ncols_data_merge)
  data_merge[1:nrows_data1, 1:ncols_data1] <- data1$data
  data_merge[1:nrows_data1, 3] <- 1

  data_merge[(nrows_data1 + 1):nrows_data_merge, 1] <- data2$data[, 1]
  data_merge[(nrows_data1 + 1):nrows_data_merge, 2] <- data2$data[, 2] + nconcepts_data1
  data_merge[(nrows_data1 + 1):nrows_data_merge, 3] <- 2
  data_merge[(nrows_data1 + 1):nrows_data_merge, 4] <- data2$data[, 4]

  data_merge[(nrows_data1 + 1):nrows_data_merge, 5:ncols_data2] <- data2$data[, 5:ncols_data2] + nconcepts_data1

  data_merge <- data_merge[order(data_merge[, 1]), ]

  #fdd - direct use of cpp
  fdd <- frequencyDistributionCpp(matrix(data_merge[, 1], length(data_merge[, 1]), 1))

  #some more intermediate processing
  ndecisionmakers <- dim(fdd)[1]

  n1 <- deparse(substitute(data1))
  n2 <- deparse(substitute(data2))

  lcovariates <- array(0, dim(concept_merge)[2])

  for (i in seq_len(dim(concept_merge)[2])) {
    lcovariates[i] <- paste("Cov", i)
  }

  #all the initial stuff packaged up
  processed <- list(data_original = NA,
                    data_name = paste0("Joined_from_", n1, "_and_", n2),
                    data = data_merge,
                    ncovariates = ncovariates_data1 + ncovariates_data2,
                    npp = dim(concept_merge)[2],
                    nmax_choiceset_size = nmaxchoicesetsize_merge,
                    ndecisionmakers = ndecisionmakers,
                    concept = concept_merge,
                    lcovariates = lcovariates,
                    fdd = fdd,
                    attribute_names  =  c(data1$attribute_names, data2$attribute_names)
  )

  return(processed)
}
