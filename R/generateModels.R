#' @export
generate_model_matrices <- function(pre_processed_data, model_type) {

  model_types <- c("fixed", "random", "one-factor", "mtmm")

  if(!(model_type %in% model_types)){
    stop(paste("model_type not one of ", paste(model_types, collapse = ', ')))
  }

  #setup values
  ncovariates <- pre_processed_data$ncovariates
  attribute_names <- pre_processed_data$attribute_names

  if(model_type == "fixed"){

    nhop<-NA

    default.muep <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    default.deltaep <- 0
    default.deltasig <- -1
    default.gamma <- rep(0,ncovariates)
    default.beta <- 0

    default.muep_initial <- rep(.1,ncovariates)
    default.musig_initial <- rep(NA,ncovariates)
    default.deltaep_initial <- rep(NA,1)
    default.deltasig_initial <- rep(NA,1)
    default.gamma_initial <- rep(NA,ncovariates)
    default.beta_initial <- rep(NA,1)

    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    gamma_model <- matrix(default.gamma, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    beta_model <- matrix(default.beta, ncol = 1, dimnames = list("HoP_1","HoP_1"))

    epsilon_model_initial <- matrix(cbind(default.muep_initial,default.musig_initial), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    delta_model_initial <- matrix(cbind(default.deltaep_initial,default.deltasig_initial), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    gamma_model_initial <- matrix(default.gamma_initial,ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    beta_model_initial <- matrix(default.beta_initial,ncol = 1, dimnames = list("HoP_1","HoP_1"))

  }else if(model_type == "random"){

    nhop <- ncovariates

    default.muep <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    default.deltaep <- rep(0,nhop)
    default.deltasig <- rep(1,nhop)
    default.gamma <- diag(nhop)*-1
    default.beta <- diag(nhop)*0

    default.muep_initial <- rep(.1,ncovariates)
    default.musig_initial <- rep(NA,ncovariates)
    default.deltaep_initial <- rep(NA,nhop)
    default.deltasig_initial <- rep(.1,nhop)
    default.gamma_initial <- diag(nhop)*NA
    default.beta_initial <- diag(nhop)*NA

    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol = 2, dimnames = list(attribute_names, c("mu","sigma")))
    delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list(paste("HoP_",1:nhop,sep=""), c("mu","sigma")))
    gamma_model <- matrix(default.gamma,ncol=nhop, dimnames = list(attribute_names, paste("HoP_",1:nhop,sep="")))
    beta_model <- matrix(default.beta,ncol=nhop, dimnames = list(paste("HoP_",1:nhop,sep=""), paste("HoP_",1:nhop,sep="")))

    epsilon_model_initial <- matrix(cbind(default.muep_initial,default.musig_initial), ncol=2, dimnames = list(attribute_names,c("mu","sigma")))
    delta_model_initial <- matrix(cbind(default.deltaep_initial,default.deltasig_initial), ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
    gamma_model_initial <- matrix(default.gamma_initial, ncol = nhop, dimnames = list(attribute_names,paste("HoP_",1:nhop,sep="")))
    beta_model_initial <- matrix(default.beta_initial, ncol = nhop, dimnames = list(paste("HoP_",1:nhop,sep=""),paste("HoP_",1:nhop,sep="")))

  }else if(model_type == "one-factor"){

    nhop<-1

    default.muep <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    default.deltaep <- rep(0,nhop)
    default.deltasig <- rep(-1,nhop)
    default.gamma <- rep(1,ncovariates)
    default.beta <- 0

    default.muep_initial <- rep(.1,ncovariates)
    default.musig_initial <- rep(NA,ncovariates)
    default.deltaep_initial <- rep(NA,1)
    default.deltasig_initial <- rep(NA,1)
    default.gamma_initial <- rep(.1,ncovariates)
    default.beta_initial <- rep(NA,1)

    epsilon_model <- matrix(cbind(default.muep,default.musig),ncol=2,dimnames = list(attribute_names,c("mu","sigma")))
    delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol = 2, dimnames = list("HoP_1",c("mu","sigma")))
    gamma_model <- matrix(default.gamma, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    beta_model<- matrix(default.beta, ncol = 1, dimnames = list("HoP_1","HoP_1"))

    epsilon_model_initial <- matrix(cbind(default.muep_initial,default.musig_initial), ncol = 2, dimnames = list(attribute_names,c("mu","sigma")))
    delta_model_initial <- matrix(cbind(default.deltaep_initial,default.deltasig_initial),ncol=2, dimnames = list("HoP_1",c("mu","sigma")))
    gamma_model_initial <- matrix(default.gamma_initial, ncol = 1, dimnames = list(attribute_names,"HoP_1"))
    beta_model_initial <- matrix(default.beta_initial, ncol = 1, dimnames = list("HoP_1","HoP_1"))



  }else if(model_type == "mtmm"){

    nhop<-ncovariates

    nhop_d2_p2<-ceiling(nhop/2)+2
    ncovariates_d2_p2<-ceiling(ncovariates/2)+2
    ncovariates_d2<-ceiling(ncovariates/2)

    default.muep <- rep(1,ncovariates)
    default.musig <- rep(0,ncovariates)
    default.deltaep <- rep(0,nhop_d2_p2)
    default.deltasig <- rep(-1,nhop_d2_p2)
    default.gamma <- rbind(cbind(diag(ncovariates_d2),matrix(cbind(rep(1,ncovariates_d2),rep(0,ncovariates_d2)),ncol=2)),
                           cbind(diag(ncovariates_d2),matrix(cbind(rep(0,ncovariates_d2),rep(1,ncovariates_d2)),ncol=2)))
    default.beta <- diag(nhop_d2_p2)*0
    default.muep_initial <- rep(.1,ncovariates)
    default.musig_initial <- rep(NA,ncovariates)
    default.deltaep_initial <- rep(NA,nhop)
    default.deltasig_initial <- rep(NA,nhop)
    default.gamma_initial <- rbind(cbind(diag(ncovariates_d2)*.1,matrix(cbind(rep(.1,ncovariates_d2),rep(NA,ncovariates_d2)),ncol=2)),
                                   cbind(diag(ncovariates_d2)*.1,matrix(cbind(rep(NA,ncovariates_d2),rep(.1,ncovariates_d2)),ncol=2)))
    default.beta_initial <- diag(nhop_d2_p2)*NA

    epsilon_model <- matrix(cbind(default.muep,default.musig), ncol=2, dimnames = list(attribute_names,c("mu","sigma")))
    delta_model <- matrix(cbind(default.deltaep,default.deltasig), ncol=2, dimnames = list(paste("HoP_",1:(nhop_d2_p2),sep=""),c("mu","sigma")))
    gamma_model <- matrix(default.gamma, ncol=ncovariates_d2_p2, dimnames = list(attribute_names, paste("HoP_",1:ncovariates_d2_p2,sep="")))
    beta_model <- matrix(default.beta, ncol=nhop_d2_p2, dimnames = list(paste("HoP_",1:(nhop_d2_p2),sep=""),paste("HoP_",1:(nhop_d2_p2),sep="")))

    epsilon_model_initial <- matrix(cbind(default.muep_initial,default.musig_initial),ncol=2,dimnames = list(attribute_names,c("mu","sigma")))
    delta_model_initial <- matrix(cbind(default.deltaep_initial,default.deltasig_initial),ncol=2, dimnames = list(paste("HoP_",1:nhop,sep=""),c("mu","sigma")))
    gamma_model_initial <- matrix(default.gamma_initial, ncol=ncovariates_d2_p2, dimnames = list(attribute_names,paste("HoP_",1:ncovariates_d2_p2,sep="")))
    gamma_model_initial[gamma_model_initial==0] <- NA
    beta_model_initial <- matrix(default.beta_initial,ncol=nhop_d2_p2, dimnames = list(paste("HoP_",1:(nhop_d2_p2),sep=""),paste("HoP_",1:(nhop_d2_p2),sep="")))
  }

  model_matrices <-list(
    epsilon_model=epsilon_model,
    delta_model=delta_model,
    gamma_model=gamma_model,
    beta_model=beta_model,
    epsilon_model_initial=epsilon_model_initial,
    delta_model_initial=delta_model_initial,
    gamma_model_initial=gamma_model_initial,
    beta_model_initial=beta_model_initial
  )

  return(model_matrices)

}


#' @export
model_generator <- function(pre_processed_data, model_type){

  matrix_list<-generate_model_matrices(pre_processed_data, model_type)
  nhop<-nrow(matrix_list$delta_model)
  npp<-processed$npp
  ncovariates<-pre_processed_data$ncovariates

  initial_e <-  matrix_list$epsilon_model_initial
  initial_e <-  na.omit(as.vector(initial_e))
  initial_d <-  matrix_list$delta_model_initial
  initial_d <-  na.omit(as.vector(initial_d))
  initial_g <-  matrix_list$gamma_model_initial
  initial_g <-  na.omit(as.vector(initial_g))
  initial_b <-  matrix_list$beta_model_initial
  initial_b <-  na.omit(as.vector(initial_b))
  initial_values <- c(initial_e,initial_d,initial_g,initial_b)

  phi <- diag(npp+nhop)

  code <- matrix(0,ncovariates,npp)
  if(ncovariates==npp){
    diag(code)<-1
  }

  description <- "" #FIX

  model<-list(description=description,
              data=pre_processed_data,
              ncovariates=ncovariates,
              npp=processed$npp,
              nhop=nhop,
              code=code,
              epsilon=matrix_list$epsilon_model,
              delta=matrix_list$delta_model,
              gamma=matrix_list$gamma_model,
              beta=matrix_list$beta_model,
              phi=phi,
              initial_values=initial_values)

  return(model)
}

















