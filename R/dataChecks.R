#precheck for multicolinearity


#' choice_model_vif
#' @param processed_data processed data list
#' @returns vif
#' @export
choice_model_vif  <-  function(processed_data) {
  #does this even make sense to do

  dataset <- processed_data$data_original[, 4:ncol(processed_data$data_original)]
  factors <- names(dataset)[sapply(dataset, is.factor)]

  if (length(factors) > 0) {

    dataset <- mltools::one_hot(data.table::as.data.table(dataset))
  }

  columns <- seq_len(ncol(dataset))

  vif <- data.frame(Variable = names(dataset), VIF = NA)

  for (i in columns){

    f <- paste(names(dataset)[i], "~.")
    lm1 <- lm(f, data = dataset)
    vif1 <- 1 / (1 - summary(lm1)$r.squared)
    vif$VIF[i] <- vif1
  }


  return(vif)
}



#other set up checks from older models
