#' @export
parPrintOld <- function(results) {

  npp <- results$model$npp
  nhop <- results$model$nhop
  parcountin <- results$par_count

  rr <- results$results


  resprint <- array(0, parcountin$total + 8)
  resprint[1] <- paste0("Printout of results for ", results$result_name, "\\\\")
  resprint[2] <- paste0("log likelihood", round(results$LL, 4), "\\\\")
  resprint[3] <- paste0("Number of parameters", parcountin$total, "\\\\")
  resprint[4] <- "$\\\\"
  resprint[5] <- "\\begin {array} {lrr}"
  resprint[6] <- paste0("Model Parameter & Estimate & Standard Error\\\\")
  resprinti <- 6

  for (i in 1:npp) {
    if (results$model$epsilon[i, 1] == 1) {
      resprinti <- resprinti + 1
      resprint[resprinti] <- paste0("\\epsilon_ {\\mu, ", i, "}&",
                                    round(rr$estimate[i], 4), "&",
                                    round(rr$standard_errors[i], 4), "\\\\")
    }
  }
  for (i in 1:nhop) {
    if (results$model$delta[i, 1] == 1) {
      resprinti <- resprinti + 1
      resprint[resprinti] <- paste0("\\delta_ {\\mu, ", i, "}&",
                                    round(rr$estimate[i], 4), "&",
                                    round(rr$standard_errors[i], 4), "\\\\")
    }
  }
  for (i in 1:npp) {
    if (results$model$epsilon[i, 2] == 1) {
      resprinti <- resprinti + 1
      resprint[resprinti] <- paste0("\\epsilon_ {\\sigma, ", i, "}&",
                                    round(rr$estimate[i], 4), "&",
                                    round(rr$standard_errors[i], 4), "\\\\")
    }
  }
  for (i in 1:nhop) {
    if (results$model$delta[i, 2] == 1) {
      resprinti <- resprinti + 1
      resprint[resprinti] <- paste0("\\delta_ {\\sigma, ", i, "}&",
                                    round(rr$estimate[i], 4), "&",
                                    round(rr$standard_errors[i], 4), "\\\\")
    }
  }
  for (i in 1:npp) {
    for (j in 1:nhop) {
      if (results$model$gamma[i, j] == 1) {
        resprinti <- resprinti + 1
        resprint[resprinti] <- paste0("\\gamma_ {", i, ", ", j, "}&",
                                      round(rr$estimate[i], 4), "&",
                                      round(rr$standard_errors[i], 4), "\\\\")
      }
    }
  }
  for (i in 1:nhop) {
    for (j in 1:nhop) {
      if (results$model$beta[i, j] == 1) {
        resprinti <- resprinti + 1
        resprint[resprinti] <- paste0("\\beta_ {", i, ", ", j, "}&",
                                      round(rr$estimate[i], 4), "&",
                                      round(rr$standard_errors[i], 4), "\\\\")
      }
    }
  }
  for (i in 1:(npp + nhop - 1)) {
    for (j in (i + 1):(npp + nhop)) {
      if (results$model$phi[i, j] == 1) {
        if (results$model$phi[j, i] == 1) {
          resprinti <- resprinti + 1
          resprint[resprinti] <- paste0("\\phi_ {", i, ", ", j, "}&",
                                        round(rr$estimate[i], 4), "&",
                                        round(rr$standard_errors[i], 4), "\\\\")
        }
      }
    }
  }
  resprinti <- resprinti + 1
  resprint[resprinti] <- "\\end {array}"
  resprinti <- resprinti + 1
  resprint[resprinti] <- "$\\\\"


  return(resprint)
}
