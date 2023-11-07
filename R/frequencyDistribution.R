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


#' Frequency distribution
#' this probably shouldnt be exported to be honest
#' @param data_column data, just a single column
#' @returns fdd
#' @export
frequencyDistribution_simple <- function(data_column) {

  cold <- matrix(data_column, length(data_column), 1)

  fdd <- frequencyDistributionCpp(cold)

  return(fdd)
}
