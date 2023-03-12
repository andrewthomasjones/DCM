#' @export
drawsMatrix <- function(ndraws, nrc, shuffle=TRUE) {

  q<-qnorm((1:(ndraws))/(ndraws+1))
  draws1 <- matrix(rep(q,nrc),ndraws,nrc)

  if(shuffle){
    draws1<- apply(draws1, 2, sample)
  }

  return(draws1)

}


#' draws <- function(nrc) {
#'
#'   draws_data<-list(
#'     draws3=draws_matrix(10^4, nrc),
#'     draws2=draws_matrix(10^3, nrc),
#'     draws1=draws_matrix(10^2, nrc),
#'     datestamp=date())
#'
#'   return(draws_data)
#' }


