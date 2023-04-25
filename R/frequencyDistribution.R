#' Frequency distribution
#'
#' @returns fdd
#' @export
frequencyDistribution <- function(cs) {

  coldd <- cs$data[, 1]
  cold <- matrix(coldd, length(coldd), 1)

  fdd <- frequencyDistributionCpp(cold)

  return(fdd)
}
