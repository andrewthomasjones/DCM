# Sets up the EMI for the fixed coefficient model

eminame=paste("./EMIs/EMI_",dataname,"_FIXED",".xlsx",sep="")

wb <- createWorkbook(eminame)

addWorksheet(wb, "epsilon_model")
  default.muep  <- rep(1,ncovariates)
  default.musig <- rep(0,ncovariates)
  epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attributenames,c("mu","sigma")))
  writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

addWorksheet(wb, "delta_model")
  default.deltaep   <- 0
  default.deltasig  <- -1
  delta_model       <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
  writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)

addWorksheet(wb, "gamma_model")
  default.gamma   <- rep(0,ncovariates)
  gamma_model     <- matrix(default.gamma, ncol = 1, dimnames = list(attributenames,"HoP_1"))
  writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)

addWorksheet(wb, "beta_model")
  default.beta    <- 0
  beta_model      <- matrix(default.beta, ncol = 1, dimnames = list("HoP_1","HoP_1"))
  writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)

addWorksheet(wb, "epsilon_initialvalues")
  default.muepinitial   <- rep(.1,ncovariates)
  default.musiginitial  <- rep(NA,ncovariates)
  epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial), ncol = 2, dimnames = list(attributenames,c("mu","sigma")))
  writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)

addWorksheet(wb, "delta_initialvalues")
  default.deltaepinitial   <- rep(NA,1)
  default.deltasiginitial  <- rep(NA,1)
  delta_modelinitial <- matrix(cbind(default.deltaepinitial,default.deltasiginitial), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
  writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)

addWorksheet(wb, "gamma_initialvalues")
  default.gammainitial  <- rep(NA,ncovariates)
  gamma_modelinitial    <- matrix(default.gammainitial,ncol = 1, dimnames = list(attributenames,"HoP_1"))
  writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

addWorksheet(wb, "beta_initialvalues")
  default.betainitial   <- rep(NA,1)
  beta_modelinitial <- matrix(default.betainitial,ncol = 1, dimnames = list("HoP_1","HoP_1"))
  writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)

saveWorkbook(wb, file = paste("./EMIs/EMI_",dataname,"_FIXED",".xlsx",sep=""), overwrite = TRUE)

cat(paste("\n","EMI model file ",substr(eminame,start = 8, stop = 999)," saved in ",getwd(),"/EMIs",sep=""))
