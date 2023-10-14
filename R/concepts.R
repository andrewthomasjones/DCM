#' create concepts.
#' @param data_matrix matrix
#' @param nmax_choiceset_size int default 31
#' @returns List of outputs .
#' @export
createConcepts <- function(data_matrix, nmax_choiceset_size = 31) {
  concepts <- createConceptsCpp(as.matrix(data_matrix), nmax_choiceset_size)
  return(concepts)
}
