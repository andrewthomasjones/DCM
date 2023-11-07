#' Frequency distribution
#' this probably shouldnt be exported to be honest
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
#' @param cs data
#' @returns fdd
#' @export
frequencyDistribution_simple <- function(coldd) {

  cold <- matrix(coldd, length(coldd), 1)

  fdd <- frequencyDistributionCpp(cold)

  return(fdd)
}

