parameterCount<-function(model){

  npp<-model$npp
  nhop<-model$nhop


  parcount<-array(0,8)

  parcount_list<-list(
    total=0,
    epsilon_mu=0,
    epsilon_sig=0,
    delta_mu=0,
    delta_sig=0,
    gamma=0,
    beta=0,
    phi=0)

  # 1 is total
  # 2 is number of epsilon mu
  # 3 is number of epsilon sig
  # 4 is number of delta mu
  # 5 is number of delta sig
  # 6 is number of gamma
  # 7 is number of beta
  # 8 is number of phi

  for (i in 1:npp){
    if (model$epsilon[i,1]==1){
      parcount[2]<-parcount[2]+1
    }
    if (model$epsilon[i,2]==1){
      parcount[3]<-parcount[3]+1
    }
  }

  for (i in 1:nhop){
    if (model$delta[i,1]==1){
      parcount[4]<-parcount[4]+1
    }
    if (model$delta[i,2]==1){
      parcount[5]<-parcount[5]+1
    }
  }

  for (i in 1:npp){
    for (j in 1:nhop){
      if (model$gamma[i,j]==1){
        parcount[6]<-parcount[6]+1
      }
    }

  }
  for (i in 1:nhop){
    for (j in 1:nhop){
      if (model$beta[i,j]==1){
        parcount[7]<-parcount[7]+1
      }
    }
  }

  for (i in 1:(npp+nhop-1)){
    for (j in (i+1):(npp+nhop)){
      if (model$phi[i,j]==1){
        if (model&phi[j,i]==1){
          parcount[8]<-parcount[8]+1
        }
      }
    }
  }

  parcount$total <- sum(parcount[2:8])

  for (i in 2:8){
    parcount_list[[i]]<-parcount[i]
  }

  return(parcount_list)
}
