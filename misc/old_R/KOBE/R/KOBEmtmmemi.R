# Sets up the EMI for the MTMM model for combined datasets

eminame=paste("./EMIs/EMI_",dataname,"_MTMM",".xlsx",sep="")

wb <- createWorkbook(eminame)

addWorksheet(wb, "epsilon_model")
  default.muep  <- rep(1,ncovariates)
  default.musig <- rep(0,ncovariates)
  epsilon_model <- matrix(cbind(default.muep,default.musig), ncol=2, dimnames = list(attributenames,c("mu","sigma")))
  writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)
  
addWorksheet(wb, "delta_model")
  nhop=ncovariates
  default.deltaep   <- rep(0,nhop/2+2)
  default.deltasig  <- rep(-1,nhop/2+2)
  delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol=2, dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),c("mu","sigma")))
  writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)
  
addWorksheet(wb, "gamma_model")
  default.gamma   <- rbind(cbind(diag(ncovariates/2)*1,matrix(cbind(rep(1,ncovariates/2),rep(0,ncovariates/2)),ncol=2)),
                           cbind(diag(ncovariates/2)*1,matrix(cbind(rep(0,ncovariates/2),rep(1,ncovariates/2)),ncol=2)))
  gamma_model <- matrix(default.gamma, ncol=(ncovariates/2)+2, dimnames = list(attributenames,paste("HoP_",1:(ncovariates/2+2),sep="")))
  writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)
  
addWorksheet(wb, "beta_model")
  default.beta   <- diag(nhop/2+2)*0
  beta_model <- matrix(default.beta,ncol=nhop/2+2,dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),paste("HoP_",1:(nhop/2+2),sep="")))
  writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)
  
addWorksheet(wb, "epsilon_initialvalues")
  default.muepinitial   <- rep(.1,ncovariates)
  default.musiginitial   <- rep(NA,ncovariates)
  epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial),ncol=2,dimnames = list(attributenames,c("mu","sigma")))
  writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)
  
addWorksheet(wb, "delta_initialvalues")
  default.deltaepinitial   <- rep(NA,nhop)
  default.deltasiginitial  <- rep(NA,nhop)
  delta_modelinitial <- matrix(cbind(default.deltaepinitial,default.deltasiginitial),ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
  writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)
  
addWorksheet(wb, "gamma_initialvalues")
  default.gammainitial   <- rbind(cbind(diag(ncovariates/2)*.1,matrix(cbind(rep(.1,ncovariates/2),rep(NA,ncovariates/2)),ncol=2)),
                                  cbind(diag(ncovariates/2)*.1,matrix(cbind(rep(NA,ncovariates/2),rep(.1,ncovariates/2)),ncol=2)))
  gamma_modelinitial <- matrix(default.gammainitial, ncol=(ncovariates/2)+2, dimnames = list(attributenames,paste("HoP_",1:(ncovariates/2+2),sep="")))
  gamma_modelinitial[gamma_modelinitial==0] <- NA
  writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)
  
addWorksheet(wb, "beta_initialvalues")
  default.betainitial   <- diag(nhop/2+2)*NA
  beta_modelinitial <- matrix(default.betainitial,ncol=nhop/2+2, dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),paste("HoP_",1:(nhop/2+2),sep="")))
  writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)
  
  saveWorkbook(wb, file = paste("./EMIs/EMI_",dataname,"_MTMM",".xlsx",sep=""), overwrite = TRUE)
  
cat(paste("\n","EMI model file ",substr(eminame,start = 8, stop = 999)," saved in ",getwd(),"/EMIs",sep=""))
