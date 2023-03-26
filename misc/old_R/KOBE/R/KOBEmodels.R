wb <- loadWorkbook("./EMIs/three_fixed.xlsx", create = TRUE)
createSheet(wb, name = "epsilon_model")
  default.muep  <- rep(1,length(colnames(datamatrix)[4:ncol(datamatrix)]))
  default.musig <- rep(0,length(colnames(datamatrix)[4:ncol(datamatrix)]))
  epsilon_model <- matrix(cbind(default.muep,default.musig),ncol=2,
                        dimnames = list(c(colnames(datamatrix)[4:ncol(datamatrix)]),c("mu","sigma")))
  writeWorksheet(wb, epsilon_model, sheet = "epsilon_model", rownames = "Attribute Names")
  setColumnWidth(wb, sheet = "epsilon_model", column = 1, width = -1)
createSheet(wb, name = "delta_model")
  default.deltaep   <- 0
  default.deltasig  <- -1
  delta_model <- matrix(cbind(default.deltaep,default.deltasig),ncol=2,
                        dimnames = list("HoP_1",c("mu","sigma")))
  writeWorksheet(wb, delta_model, sheet = "delta_model", rownames = "")
  setColumnWidth(wb, sheet = "delta_model", column = 1, width = -1)
createSheet(wb, name = "gamma_model")
createSheet(wb, name = "beta_model")
createSheet(wb, name = "epsilon_initialvalues")
createSheet(wb, name = "delta_initialvalues")
createSheet(wb, name = "gamma_initialvalues")
createSheet(wb, name = "beta_initialvalues")
saveWorkbook(wb)


