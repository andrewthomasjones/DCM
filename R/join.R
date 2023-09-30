#' join
#' @param data1 processed data list
#' @param data2 processed data list
#' @returns joins two processed objects
#' @export
join_choicedatasets  <-  function(data1,  data2) {

  #from each input
  data_1 <- data1$data_original
  data_2 <- data2$data_original

  if (ncol(data_1) == ncol(data_2)) {
    data_original <- data.frame(Map(c, data_1, data_2))
  }else {
    print("ERROR - datasets to be joined need to have the same number of atributes.")
    return(NA)
  }

  #from Kobe code
  nmax_choiceset_size <- as.numeric(max(unlist(rle(data_original[, 2])[1])))

  #concept list
  concept_list <- createConcepts(data_original, nmax_choiceset_size)

  #fdd
  fdd <- frequencyDistribution(concept_list)

  #some more intermediate processing
  ndecisionmakers <- dim(fdd)[1]

  lcovariates <- array(0, concept_list$ncovariates)

  for (i in 1:concept_list$ncovariates) {
    lcovariates[i] <- paste("Cov", i)
  }

  n1 <- deparse(substitute(data1))
  n2 <- deparse(substitute(data2))

  #all the initial stuff packaged up
  processed <- list(data_original = data_original,
                    data_name = paste0("Joined_from_", n1, "_and_", n2),
                    data = concept_list$data,
                    ncovariates = concept_list$ncovariates,
                    npp = concept_list$ncovariates,
                    nmax_choiceset_size = nmax_choiceset_size,
                    ndecisionmakers = ndecisionmakers,
                    concept = concept_list$concept,
                    lcovariates = lcovariates,
                    fdd = fdd,
                    attribute_names  =  names(data_original)[-(1:3)]
  )

  return(processed)
}
