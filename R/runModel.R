#' @export
runModel <- function(model, model_name="name", ndraws=1000){

  parcount<-parameterCount(model)
  shuffle<-FALSE
  processed<-model$data

  model_name<-model$description

  npp<-model$npp
  nhop<-model$nhop

  # if (parcount[[8]]==0){
  #   print('correlations not operational yet')
  #   return(NA)
  # }

  if (length(model$initial_values)==parcount$total){
    print('You have the correct number of initial values.')
  }else{
    print('ERROR - you have the incorrect number of initial values.')
    return(NA)
  }

  nrc<-dim(model$epsilon)[1]+dim(model$delta)[1]
  draws_matrix<-drawsMatrix(ndraws, nrc, shuffle)

    loglik1<-suppressWarnings(llMax2(model, processed, draws_matrix))

    standard_errors <- sqrt(diag(solve(loglik1$hessian)))

    printpara <- matrix(0, 7, 1)
    row.names(printpara) <- c("epsilon_mu","epsilon_sig","delta_mu","delta_sig","gamma","beta","phi")

    for (i in 1:npp){
      if (model$epsilon[i,1]==1){printpara[1]=printpara[1]+1}
      if (model$epsilon[i,2]==1){printpara[2]=printpara[2]+1}
    }

    for (i in 1:nhop){
      if (model$delta[i,1]==1){printpara[3]=printpara[3]+1}
      if (model$delta[i,2]==1){printpara[4]=printpara[4]+1}
    }

    for (i in 1:npp){
      for (j in 1:nhop){
        if (model$gamma[i,j]==1){printpara[5]=printpara[5]+1}
      }
    }

    for (i in 1:nhop){
      for (j in 1:nhop){
        if (model$beta[i,j]==1){printpara[6]=printpara[6]+1}
      }
    }

    k<-parcount$total
    n<-nrow(processed$data)
    AIC <- 2*k -2*log(loglik1$minimum)
    BIC <- k*log(n) - 2*log(loglik1$minimum)


    parastems <- c(rep(("epsilon"),printpara[1]), rep(("epsilon_sig"),printpara[2]), rep(("delta_mu"),printpara[3]), rep(("delta_sig"),printpara[4]), rep(("gamma"),printpara[5]), rep(("beta"),printpara[6]))

    subscripts <- matrix(rbind(which(model$epsilon >0, arr.ind = T), which(model$delta >0, arr.ind = T), which(model$gamma >0, arr.ind = T), which(model$beta >0, arr.ind = T)),nrow = sum(printpara), ncol = 2)

    parameters <- paste0(parastems,"_","[",subscripts[,1],",",subscripts[,2],"]")

    results <- data.frame(parameters=parameters, estimate=loglik1$estimate, standard_errors=standard_errors)

    results$LL <-  c(loglik1$minimum, rep(".", nrow(results)-1))

    result_name<-paste0(model_name, " ", format(Sys.time(), "%Y-%m-%d %H:%M"))

    fitted_model <- list(result_name=result_name,  model=model, model_name=model_name, LL=loglik1$minimum, loglikf=loglik1, ndraws=ndraws, results=results, AIC = AIC, BIC=BIC, par_count=parcount)

  return(fitted_model)
}
