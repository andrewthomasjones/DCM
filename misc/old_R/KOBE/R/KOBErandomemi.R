# Sets up the EMI for the random coefficient model

eminame=paste("./EMIs/EMI_",dataname,"_RANDOM",".xlsx",sep="")

wb <- createWorkbook(eminame)

addWorksheet(wb, "epsilon_model")
  default.muep  <- rep(1,ncovariates)
  default.musig <- rep(0,ncovariates)
  epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attributenames, c("mu","sigma")))
  writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

addWorksheet(wb, "delta_model")
  nhop=ncovariates
  default.deltaep   <- rep(0,nhop)
  default.deltasig  <- rep(1,nhop)
  delta_model       <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list(paste("HoP_",1:nhop,sep=""), c("mu","sigma")))
  writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)
  
addWorksheet(wb, "gamma_model")
  default.gamma   <- diag(nhop)*-1
  gamma_model     <- matrix(default.gamma,ncol=nhop, dimnames = list(attributenames, paste("HoP_",1:nhop,sep="")))
  writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)
  
addWorksheet(wb, "beta_model")
  default.beta    <- diag(nhop)*0
  beta_model      <- matrix(default.beta,ncol=nhop, dimnames = list(paste("HoP_",1:nhop,sep=""), paste("HoP_",1:nhop,sep="")))
  writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)
  
addWorksheet(wb, "epsilon_initialvalues")
  default.muepinitial   <- rep(.1,ncovariates)
  default.musiginitial  <- rep(NA,ncovariates)
  epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial), ncol=2, dimnames = list(attributenames,c("mu","sigma")))
  writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)
  
addWorksheet(wb, "delta_initialvalues")
  default.deltaepinitial   <- rep(NA,nhop)
  default.deltasiginitial  <- rep(.1,nhop)
  delta_modelinitial       <- matrix(cbind(default.deltaepinitial,default.deltasiginitial), ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
  writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)
  
addWorksheet(wb, "gamma_initialvalues")
  default.gammainitial   <- diag(nhop)*NA
  gamma_modelinitial     <- matrix(default.gammainitial, ncol = nhop, dimnames = list(attributenames,paste("HoP_",1:nhop,sep="")))
  writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

addWorksheet(wb, "beta_initialvalues")
  default.betainitial   <- diag(nhop)*NA
  beta_modelinitial     <- matrix(default.betainitial, ncol = nhop, dimnames = list(paste("HoP_",1:nhop,sep=""),paste("HoP_",1:nhop,sep="")))
  writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)
  
saveWorkbook(wb, file = paste("./EMIs/EMI_",dataname,"_RANDOM",".xlsx",sep=""), overwrite = TRUE)
  
cat(paste("\n","EMI model file ",substr(eminame,start = 8, stop = 999)," saved in ",getwd(),"/EMIs",sep=""))
