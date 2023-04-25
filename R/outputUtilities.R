#' @export
summariseModelList <- function(list_of_models) {

  m <- length(list_of_models)

  table <- tibble::tibble("Model Specifications" = rep(NA, m),
             "Parameters" = rep(NA, m),
             "Latent Variables" = rep(NA, m),
              "Log-Likelihood" = rep(NA, m),
              "AIC" = rep(NA, m),
              "BIC" = rep(NA, m)
             )

  for (i in 1:m) {
     table$"Model Specifications"[i] <- paste0("M",  i,  ": ",  list_of_models[[i]]$model_name)
     table$"Parameters"[i] <- list_of_models[[i]]$par_count$total
     table$"Latent Variables"[i] <- NA
     table$"Log-Likelihood"[i] <-  list_of_models[[i]]$LL
     table$"AIC"[i] <- list_of_models[[i]]$AIC
     table$"BIC"[i] <- list_of_models[[i]]$BIC
  }


  return(table)
}


#' @export
modelSummary <- function(model) {

}

