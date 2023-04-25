#' Draws matrix
#'
#' @returns draws matrix
#' @export
drawsMatrix <- function(ndraws, nrc, shuffle = TRUE) {

  draws_range <- (1:(ndraws)) / (ndraws + 1)
  q <- qnorm(draws_range)
  draws1 <- matrix(rep(q, nrc), ndraws, nrc)

  if (shuffle) {
    draws1 <- apply(draws1, 2, sample)
  }

  return(draws1)

}
