llMax <-function(parcount, model, processed, drawsmatrix){

  loglik <- nlm(llCalc, p=initialvalues,
                model,
                processed$concept,
                processed$nmaxchoicesetsize,
                processed$data,
                processed$ndecisionmakers,
                drawsmatrix,
                hessian = TRUE)

  return(loglik)
}

llCalc<-function(workingvalues,model,concept,nmaxchoicesetsize,data,ndecisionmakers,drawsmatrix){

  epsilonmatrix<-model$epsilon
  deltamatrix<-model$delta
  gammamatrix<-model$gamma
  deltamatrix<-model$delta
  betamatrix<-model$beta
  phimatrix<-model$phi

  npp<-model$npp
  nhop<-model$nhop
  ndraws<-dim(drawsmatrix)[1]

  muepsilonparameters<-array(0,npp)
  mudeltaparameters<-array(0,nhop)
  sigmaepsilonparameters<-array(0,npp)
  sigmadeltaparameters<-array(0,nhop)

  gammaparameters<-gammamatrix*0
  betaparameters<-betamatrix*0
  phiparameters<-phimatrix*0

  m<-0

  for(i in 1:npp){

    if(epsilonmatrix[i,1]==1){
      m<-m+1
      muepsilonparameters[i]<-workingvalues[m]
    }

    if(epsilonmatrix[i,1]==-1) muepsilonparameters[i]<-1
  }

  for(i in 1:nhop){

    if(deltamatrix[i,1]==1){

      m<-m+1
      mudeltaparameters[i]<-workingvalues[m]
    }

    if(deltamatrix[i,1]==-1){
      deltaepsilonparameters[i]<-1
    }
  }

  for(i in 1:npp){

    if(epsilonmatrix[i,2]==1){
      m<-m+1
      sigmaepsilonparameters[i]<-abs(workingvalues[m])
    }

    if(epsilonmatrix[i,2]==-1){
      sigmaepsilonparameters[i]<-1
    }
  }

  for(i in 1:nhop){

    if(deltamatrix[i,2]==1){
      m<-m+1
      sigmadeltaparameters[i]<-abs(workingvalues[m])
    }

    if(deltamatrix[i,2]==-1){
      sigmadeltaparameters[i]<-1
    }
  }

  for(j in 1: nhop){
    for(i in 1:npp){
      if(gammamatrix[i,j]==1){
        m<-m+1
        gammaparameters[i,j]<-workingvalues[m]
      }
      if(gammamatrix[i,j]==-1){
        gammaparameters[i,j]<-1
      }
    }
  }
  for(j in 1:nhop){
    for(i in 1:nhop){
      if(betamatrix[i,j]==1){
        m<-m+1
        betaparameters[i,j]<-workingvalues[m]
      }
      if(betamatrix[i,j]==-1){
        betaparameters[i,j]<-1
      }
    }
  }


  diag(phiparameters)<- 1


  for(i in 1:(npp+nhop-1)){
    for(j in (i+1):(npp+nhop)){
      if(phimatrix[i,j]==1){
        if(phimatrix[j,i]==1){
          m<-m+1
          phiparameters[i,j]<-workingvalues[m]
          phiparameters[j,i]<-workingvalues[m]
        }
      }
    }
  }

  drawsmatrix<-drawsmatrix%*%(phiparameters^0.5)
  drawsepsilon<-cbind(drawsmatrix[,1])

  if(npp>1){
    for(i in 2:npp){
      drawsepsilon<-cbind(drawsepsilon,drawsmatrix[,i])
    }
  }

  drawsdelta<-cbind(drawsmatrix[,npp+1])

  if(nhop>1){
    for(i in 2:nhop){
      drawsdelta<-cbind(drawsdelta,drawsmatrix[,i+npp])
    }
  }

  for(i in 1:ndraws){
    for(j in 1:npp){
      drawsepsilon[i,j]<-drawsepsilon[i,j]*sigmaepsilonparameters[j]+muepsilonparameters[j]
    }
    for(j in 1:nhop){
      drawsdelta[i,j]<-drawsdelta[i,j]*sigmadeltaparameters[j]+mudeltaparameters[j]
    }
  }

  imatrix<-matrix(0,nhop,nhop)
  diag(imatrix)<- 1

  gb<-imatrix-betaparameters
  gb<-solve(gb)
  gb <- gammaparameters%*%gb
  gb <- gb%*%t(drawsdelta)
  gb<-gb+t(drawsepsilon)
  conceptuse<-concept%*%model$code
  gb<-conceptuse%*%gb
  gb<-exp(gb)

  pthisdm <- matrix(1,1,ndraws)
  pthiscs <- matrix(0,1,ndraws)


  ploglike <- array(0, ndecisionmakers)
  n <- 1
  iddm <- data[1,1]
  bottom <- array(0, ncol(gb))

  nlines <- dim(data)[1]

  for(i in 1:nlines){
    bottom <- bottom*0

    for(j in 1:nmaxchoicesetsize){
      if(data[i,j+4]>0){
        bottom <- bottom+gb[data[i,j+4],]
      }
    }

    pthiscs <- gb[data[i,2],]/bottom

    if(data[i,1]==iddm){
      pthisdm <- pthisdm*pthiscs
    }
    if(data[i,1]>iddm){
      ploglike[n]<-sum(pthisdm)/ndraws
      n <- n+1
      pthisdm <- pthiscs
      iddm <- data[i,1]
    }
  }

  ploglike[n]<-sum(pthisdm)/ndraws

  ploglike <- log(ploglike)
  loglike <- sum(ploglike)
  loglike <- -loglike


  return(loglike)
}

