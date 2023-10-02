#' create_emi_file
#'
#' @param file_name processed data list
#' @param model_type type for base model string
#' @param ncovariates need this or name list
#' @param attribute_names need this ncovariates
#' @param verbose prints more if >0
#' @returns creates file
#' @export
create_emi_file  <-  function(file_name, model_type, ncovariates = NULL, attribute_names = NULL, verbose = 1)  {

  #various checks

  model_types  <-  c("fixed",  "random",  "one-factor", "mtmm")

  if (!(model_type %in% model_types)) {
    stop(paste("model_type not one of ",  paste(model_types,  collapse = ",  ")))
  }

  if(is.null(ncovariates) & is.null(attribute_names)){
    stop("Need to provide at least one of ncovariates and attribute_names")
  }

  if(is.null(ncovariates) & !is.null(attribute_names)){
    ncovariates <- length(attribute_names)
  }

  if(is.null(attribute_names) & !is.null(ncovariates) ){
    attribute_names = paste0("V", 1:ncovariates)
  }

  if(!is.null(attribute_names) & !is.null(ncovariates) ){
    if(length(attribute_names) != ncovariates){
      stop("attribute_names length must equal ncovariates")
    }
  }


  wb <- openxlsx::createWorkbook(file_name)

  if(model_type == "fixed"){

    openxlsx::addWorksheet(wb, "epsilon_model")
    default.muep  <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_model")
    default.deltaep   <- 0
    default.deltasig  <- -1
    delta_model       <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_model")
    default.gamma   <- rep(0,ncovariates)
    gamma_model     <- matrix(default.gamma, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    openxlsx::writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_model")
    default.beta    <- 0
    beta_model      <- matrix(default.beta, ncol = 1, dimnames = list("HoP_1","HoP_1"))
    openxlsx::writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "epsilon_initialvalues")
    default.muepinitial   <- rep(.1,ncovariates)
    default.musiginitial  <- rep(NA,ncovariates)
    epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_initialvalues")
    default.deltaepinitial   <- rep(NA,1)
    default.deltasiginitial  <- rep(NA,1)
    delta_modelinitial <- matrix(cbind(default.deltaepinitial,default.deltasiginitial), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    openxlsx::writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_initialvalues")
    default.gammainitial  <- rep(NA,ncovariates)
    gamma_modelinitial    <- matrix(default.gammainitial,ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    openxlsx::writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_initialvalues")
    default.betainitial   <- rep(NA,1)
    beta_modelinitial <- matrix(default.betainitial,ncol = 1, dimnames = list("HoP_1","HoP_1"))
    openxlsx::writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)

  }else if(model_type == "random"){

    openxlsx::addWorksheet(wb, "epsilon_model")
    default.muep  <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attribute_names, c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_model")
    nhop<-ncovariates
    default.deltaep   <- rep(0,nhop)
    default.deltasig  <- rep(1,nhop)
    delta_model       <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list(paste("HoP_",1:nhop,sep=""), c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_model")
    default.gamma   <- diag(nhop)*-1
    gamma_model     <- matrix(default.gamma,ncol=nhop, dimnames = list(attribute_names, paste("HoP_",1:nhop,sep="")))
    openxlsx::writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_model")
    default.beta    <- diag(nhop)*0
    beta_model      <- matrix(default.beta,ncol=nhop, dimnames = list(paste("HoP_",1:nhop,sep=""), paste("HoP_",1:nhop,sep="")))
    openxlsx::writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "epsilon_initialvalues")
    default.muepinitial   <- rep(.1,ncovariates)
    default.musiginitial  <- rep(NA,ncovariates)
    epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial), ncol=2, dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_initialvalues")
    default.deltaepinitial   <- rep(NA,nhop)
    default.deltasiginitial  <- rep(.1,nhop)
    delta_modelinitial       <- matrix(cbind(default.deltaepinitial,default.deltasiginitial), ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
    openxlsx::writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_initialvalues")
    default.gammainitial   <- diag(nhop)*NA
    gamma_modelinitial     <- matrix(default.gammainitial, ncol = nhop, dimnames = list(attribute_names,paste("HoP_",1:nhop,sep="")))
    openxlsx::writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_initialvalues")
    default.betainitial   <- diag(nhop)*NA
    beta_modelinitial     <- matrix(default.betainitial, ncol = nhop, dimnames = list(paste("HoP_",1:nhop,sep=""),paste("HoP_",1:nhop,sep="")))
    openxlsx::writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)

  }else if(model_type == "one-factor"){

    openxlsx::addWorksheet(wb, "epsilon_model")
    default.muep  <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    epsilon_model <- matrix(cbind(default.muep,default.musig),ncol=2,dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_model")
    nhop <- 1
    default.deltaep   <- rep(0,nhop)
    default.deltasig  <- rep(-1,nhop)
    delta_model       <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_model")
    default.gamma     <- rep(1,ncovariates)
    gamma_model       <- matrix(default.gamma, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    openxlsx::writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_model")
    default.beta      <- 0
    beta_model        <- matrix(default.beta, ncol = 1, dimnames = list("HoP_1","HoP_1"))
    openxlsx::writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "epsilon_initialvalues")
    default.muepinitial   <- rep(.1,ncovariates)
    default.musiginitial  <- rep(NA,ncovariates)
    epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_initialvalues")
    default.deltaepinitial   <- rep(NA,1)
    default.deltasiginitial  <- rep(NA,1)
    delta_modelinitial       <- matrix(cbind(default.deltaepinitial,default.deltasiginitial),ncol=2, dimnames = list("HoP_1",c("mu","sigma")))
    openxlsx::writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_initialvalues")
    default.gammainitial   <- rep(.1,ncovariates)
    gamma_modelinitial     <- matrix(default.gammainitial, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    openxlsx::writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_initialvalues")
    default.betainitial   <- rep(NA,1)
    beta_modelinitial     <- matrix(default.betainitial, ncol = 1, dimnames = list("HoP_1","HoP_1"))
    openxlsx::writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)

  }else if(model_type == "mtmm"){

    openxlsx::addWorksheet(wb, "epsilon_model")
    default.muep  <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol=2, dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "epsilon_model", epsilon_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_model")
    nhop <- ncovariates
    default.deltaep   <- rep(0,nhop/2+2)
    default.deltasig  <- rep(-1,nhop/2+2)
    delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol=2, dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),c("mu","sigma")))
    openxlsx::writeData(wb, sheet = "delta_model", delta_model, rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_model")
    default.gamma   <- rbind(cbind(diag(ncovariates/2)*1,matrix(cbind(rep(1,ncovariates/2),rep(0,ncovariates/2)),ncol=2)),
                             cbind(diag(ncovariates/2)*1,matrix(cbind(rep(0,ncovariates/2),rep(1,ncovariates/2)),ncol=2)))
    gamma_model <- matrix(default.gamma, ncol=(ncovariates/2)+2, dimnames = list(attribute_names,paste("HoP_",1:(ncovariates/2+2),sep="")))
    openxlsx::writeData(wb, gamma_model, sheet = "gamma_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_model")
    default.beta   <- diag(nhop/2+2)*0
    beta_model <- matrix(default.beta,ncol=nhop/2+2,dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),paste("HoP_",1:(nhop/2+2),sep="")))
    openxlsx::writeData(wb, beta_model, sheet = "beta_model", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "epsilon_initialvalues")
    default.muepinitial   <- rep(.1,ncovariates)
    default.musiginitial   <- rep(NA,ncovariates)
    epsilon_modelinitial  <- matrix(cbind(default.muepinitial,default.musiginitial),ncol=2,dimnames = list(attribute_names,c("mu","sigma")))
    openxlsx::writeData(wb, epsilon_modelinitial, sheet = "epsilon_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "delta_initialvalues")
    default.deltaepinitial   <- rep(NA,nhop)
    default.deltasiginitial  <- rep(NA,nhop)
    delta_modelinitial <- matrix(cbind(default.deltaepinitial,default.deltasiginitial),ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
    openxlsx::writeData(wb, delta_modelinitial, sheet = "delta_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "gamma_initialvalues")
    default.gammainitial   <- rbind(cbind(diag(ncovariates/2)*.1,matrix(cbind(rep(.1,ncovariates/2),rep(NA,ncovariates/2)),ncol=2)),
                                    cbind(diag(ncovariates/2)*.1,matrix(cbind(rep(NA,ncovariates/2),rep(.1,ncovariates/2)),ncol=2)))
    gamma_modelinitial <- matrix(default.gammainitial, ncol=(ncovariates/2)+2, dimnames = list(attribute_names,paste("HoP_",1:(ncovariates/2+2),sep="")))
    gamma_modelinitial[gamma_modelinitial==0] <- NA
    openxlsx::writeData(wb, gamma_modelinitial, sheet = "gamma_initialvalues", rowNames = TRUE)

    openxlsx::addWorksheet(wb, "beta_initialvalues")
    default.betainitial   <- diag(nhop/2+2)*NA
    beta_modelinitial <- matrix(default.betainitial,ncol=nhop/2+2, dimnames = list(paste("HoP_",1:(nhop/2+2),sep=""),paste("HoP_",1:(nhop/2+2),sep="")))
    openxlsx::writeData(wb, beta_modelinitial, sheet = "beta_initialvalues", rowNames = TRUE)
  }

  openxlsx::saveWorkbook(wb, file = file_name, overwrite = TRUE)

  if(verbose>0){
    message(paste("\n",model_type, "EMI model file saved as", file_name,sep=""))
  }

}
