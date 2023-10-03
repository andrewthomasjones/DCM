#' vif_check
#'
#' @param data precprocessed data
#' @returns vif
#' @export
vif_check <- function(data) {

  predictors <- data$data_original[, -(1:3)]

  predictors2 <- predictors

  vals <- list()

  for (i in 1:ncol(predictors2)) {
      a <- lm(as.formula(paste(colnames(predictors2)[i], "~ .")), data=predictors2)
      r <- summary(a)$r.squared
      vif <- 1 / (1 - r)
      vals[[i]] <- vif
  }

  return(data.frame(Variable = names(predictors2), VIF=unlist(vals)))
}

#' cor_mat
#'
#' @param data precprocessed data
#' @returns cor mat
#' @export
cor_mat <- function(data) {

  predictors <- data$data_original[, -(1:3)]

  r<-cor(as.matrix(predictors), method = "pearson")
  m<-round(r, 4)
  return(m)
}

#' cor_sig
#'
#' @param data precprocessed data
#' @returns sor signficiance
#' @export
cor_sig <- function(data) {
  predictors <- data$data_original[, -(1:3)]
  cm <- cor(as.matrix(predictors), method = "pearson")
  res <- Hmisc::rcorr(cm)
  return(res)
}

