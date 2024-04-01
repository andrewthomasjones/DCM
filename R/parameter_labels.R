#' Create parameter labels
#' @param model model list
#' @returns list of parmeter names
#' @export
parameter_labels <- function(model){

  npp <- model$npp
  nhop <- model$nhop

  printpara  <-  matrix(0,  7,  1)

  row.names(printpara)  <-  c("epsilon_mu",
                              "epsilon_sig",
                              "delta_mu",
                              "delta_sig",
                              "gamma",
                              "beta",
                              "phi")

  for (i in 1:npp) {
    if (model$epsilon[i, 1] == 1) {
      printpara[1] <- printpara[1] + 1
    }
    if (model$epsilon[i, 2] == 1) {
      printpara[2] <- printpara[2] + 1
    }
  }

  for (i in 1:nhop) {
    if (model$delta[i, 1] == 1) {
      printpara[3] <- printpara[3] + 1
    }
    if (model$delta[i, 2] == 1) {
      printpara[4] <- printpara[4] + 1
    }
  }

  for (i in 1:npp) {
    for (j in 1:nhop) {
      if (model$gamma[i, j] == 1) {
        printpara[5] <- printpara[5] + 1
      }
    }
  }

  for (i in 1:nhop) {
    for (j in 1:nhop) {
      if (model$beta[i, j] == 1) {
        printpara[6] <- printpara[6] + 1
      }
    }
  }



  para_stems  <-  c(rep(("epsilon"),
                        printpara[1]),
                    rep(("epsilon_sig"),
                        printpara[2]),
                    rep(("delta_mu"),
                        printpara[3]),
                    rep(("delta_sig"),
                        printpara[4]),
                    rep(("gamma"),
                        printpara[5]),
                    rep(("beta"),
                        printpara[6]))

  subscripts  <-  matrix(rbind(which(model$epsilon > 0,  arr.ind = TRUE),
                               which(model$delta > 0,  arr.ind = TRUE),
                               which(model$gamma > 0,  arr.ind = TRUE),
                               which(model$beta > 0,  arr.ind = TRUE)),
                         nrow = sum(printpara),  ncol = 2)

  parameters  <-  paste0(para_stems, "_",
                         "[", subscripts[, 1], ", ", subscripts[, 2], "]")

  return(parameters)
}
