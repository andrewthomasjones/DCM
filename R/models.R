#' @export
model_generator_old <- function(processed, nhop, description, type){

  npp<-processed$npp

  ncovariates<-processed$ncovariates

  code <- matrix(0,ncovariates,npp)

  if(type=="Fixed"){
    epsilon <- matrix(c(array(1, npp), array(0, npp)),npp,2)
  }

  if(type=="Random"){
    epsilon <- matrix(1,npp,2)
  }

  if(type=="Global"){
    epsilon <- matrix(c(array(1, npp), array(0, npp)),npp,2)
  }

  delta <- matrix(0,nhop,2)
  if(type=="Global"){
    delta[1,2]<- -1
  }


  gamma <- matrix(0,npp,nhop)
  if(type=="Global"){
    gamma[,1] <- 1
  }


  beta <- matrix(0,nhop,nhop)
  phi <- diag(npp+nhop)

  if (ncovariates==npp){
    diag(code)<-1
  }

  if(type=="Fixed"){
    initial_values<-array(0.1, npp)
  }
  if(type=="Random"){
    initial_values<-array(0.1, 2*npp)
  }

  if(type=="Global"){
    initial_values<-array(0.1, 2*npp)
  }

  model<-list(description=description,
              data=processed,
              ncovariates=ncovariates,
              npp=npp,
              nhop=nhop,
              code=code,
              epsilon=epsilon,
              delta=delta,
              gamma=gamma,
              beta=beta,
              phi=phi,
              initial_values=initial_values)

  return(model)
}










