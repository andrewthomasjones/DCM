library(DCM)

processedDCE <- setUp(DCEpriorities)
test2 <- removeVariables(processedDCE, c("Safety_DCE"), verbose = T)


processedDCE <- setUp(DCEpriorities[,1:6])
processedBW <- setUp(BWpriorities[,1:6])
test1 <- joinChoiceDatasets(processedBW, processedDCE)


model_1f2 <- modelGenerator(test2, "one-factor")
model_fixed2 <- modelGenerator(test2, "fixed")
model_random2 <- modelGenerator(test2, "random")

res_1f2 <- runModel(model_1f2)
res_fixed2 <- runModel(model_fixed2)
res_random2 <- runModel(model_random2)


model_1f <- modelGenerator(test1, "one-factor")
model_fixed <- modelGenerator(test1, "fixed")
model_random <- modelGenerator(test1, "random")
model_mtmm <- modelGenerator(test1, "mtmm")

res_1f <- runModel(model_1f)
res_fixed <- runModel(model_fixed)
res_random <- runModel(model_random)
res_mtmm  <- runModel(model_mtmm)


fixed_model_graph(res_fixed) %>% DiagrammeR::render_graph()

test1 <- selectVariables(processedDCE, c("Safety_DCE", "Reliability_DCE"), verbose = T)


joined2 <- removeVariables(joined, c("Safety_DCE", "Safety_BW"))

joined3 <- selectVariables(joined, c("Safety_DCE", "Safety_BW"), verbose = T)





model_1f <- modelGenerator(processedBW, "one-factor")
model_fixed <- modelGenerator(processedBW, "fixed")
model_random <- modelGenerator(processedBW, "random")
model_mtmm <- modelGenerator(joined, "mtmm")

res_1f <- runModel(model_1f)
res_fixed <- runModel(model_fixed)
res_random <- runModel(model_random)
res_mtmm  <- runModel(model_mtmm)



manual_example <- list(
  "epsilon_model" = matrix(c(1, 1, 1, 0, 0 ,0), ncol = 2),
  "delta_model"   = matrix(c(0, -1), ncol = 2),
  "gamma_model"   = matrix(c(0, 0, 0), ncol = 1),
  "beta_model"    = matrix(c(0), ncol = 1, nrow = 1),

  "epsilon_model_initial" = matrix(c(.1, .2, .1, NA, NA, NA), ncol = 2),
  "delta_model_initial"   = matrix(c(NA, NA), ncol = 2),
  "gamma_model_initial"   = matrix(c(NA, NA, NA), ncol = 1),
  "beta_model_initial"    = matrix(NA, ncol = 1, nrow = 1)
)


model_manual <- modelGenerator(processedBW,
               model_type = "manual",
               matrix_list = manual_example)

res_manual  <- runModel(model_manual)





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









processedDCE <- setUp(DCEpriorities[,1:6])

model <- modelGenerator(processedDCE, "random")
res <- runModel(model, dev_mode = "TMB", scaling = TRUE)

resF <- runModel(model, dev_mode = "TMB", scaling = FALSE)


res2 <- runModel(model)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBW <- removeVariables(processedBW , "Accessibility_BW" )

processedDCE <- setUp(DCEpriorities)
joined <- joinChoiceDatasets (processedBW, processedDCE)

model_1f <- modelGenerator(processedBW, "one-factor")
model_fixed <- modelGenerator(processedBW, "fixed")
model_random <- modelGenerator(processedBW, "random")
model_mtmm <- modelGenerator(joined, "mtmm")

res_1f <- runModel(model_1f)
res_fixed <- runModel(model_fixed)
res_random <- runModel(model_random)

res_1f2 <- runModel(model_1f, dev_mode = "TMB")
res_fixed2 <- runModel(model_fixed, dev_mode = "TMB")
res_random2<- runModel(model_random, dev_mode = "TMB")

res_mtmm  <- runModel(model_mtmm)
res_mtmm2  <- runModel(model_mtmm, dev_mode = "TMB")

round(res_1f$results$estimate,1)
round(res_fixed$results$estimate,1)
round(res_random$results$estimate,1)
round(res_mtmm$results$estimate,1)

processed <- removeVariables(processed, "Accessibility_BW" )
model<- modelGenerator(processed, "fixed")
res <- runModel(model)
chosen_values <- res$results$estimate



s1 <- scores(chosen_values,  model,  processed)

a <- t(s1) %*% s1

b <- solve(res$loglikf$hessian) %*% a %*% solve(res$loglikf$hessian)
sqrt(diag(b))
sqrt(diag(solve(res$loglikf$hessian)))


processedDCE <- setUp(DCEpriorities)
processedBW <- setUp(BWpriorities)

#joined <- joinChoiceDatasets (processedBWr , processedDCE)

gt <- 1e-6
st <- 1e-6

model_fixed <- modelGenerator(processedDCE, "fixed")
test_fixed_C_DCE <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)


standard_errors <- sqrt(diag(solve(test_fixed_C_DCE$loglikf$hessian)))
standard_errors



delta_grid <- suppressMessages(mvQuad::createNIGrid(dim = model_fixed$npp + model_fixed$nhop,
                                                    type = "GHN",
                                                    level = 3,
                                                    ndConstruction = "sparse"))

ghq_matrix1 <- as.matrix(cbind(delta_grid$weights, delta_grid$nodes))

hessian2 <- numDeriv::hessian(func = llCalc,
                              x = test_fixed_C_DCE$loglikf$estimate,
                              method.args=list(eps=1e-4,
                                               d=0.01, zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2, show.details=TRUE),
                              model = model_fixed,
                              processed = model_fixed$data,
                              ghq_matrix1 = as.matrix(ghq_matrix1)
)




standard_errors <- sqrt(diag(solve(hessian2)))
standard_errors




solve(hessian2) %*% %*% solve(hessian2)








model_fixed <- modelGenerator(processedBW, "fixed")
test_fixed_R_BW <- runModel(model_fixed,  dev_mode = "R",  ghq_size = 3, verbose = 0, gradtol = gt, steptol = st)
test_fixed_R_BW$results

model_fixed <- modelGenerator(processedDCE, "fixed")
test_fixed_R_DCE <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)

model_fixed <- modelGenerator(joined, "fixed")
test_fixed_R_BWDCE <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)





test_fixed_C_BW <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)

error <- abs(test_fixed_R_BW$loglikf$hessian - test_fixed_C_BW$loglikf$hessian) / test_fixed_R_BW$loglikf$hessian
error[error < 10^-3] <- 0
error

abs(test_fixed_C_BW$LL - test_fixed_R_BW$LL) / test_fixed_R_BW$LL

standard_errors <- sqrt(diag(solve(test_fixed_R_BW$loglikf$hessian)))
standard_errors

delta_grid <- suppressMessages(mvQuad::createNIGrid(dim = model_fixed$npp + model_fixed$nhop,
                                                    type = "GHN",
                                                    level = 3,
                                                    ndConstruction = "sparse"))

ghq_matrix1 <- as.matrix(cbind(delta_grid$weights, delta_grid$nodes))

hessian2 <- numDeriv::hessian(func = llCalc,
                              x = test_fixed_R_BW$loglikf$estimate,
                              method.args=list(eps=1e-4,
                                               d=0.01, zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2, show.details=TRUE),
                              model = model_fixed,
                              processed = model_fixed$data,
                              ghq_matrix1 = as.matrix(ghq_matrix1)
)

standard_errors2 <- sqrt(diag(solve(hessian2)))
standard_errors2
test_fixed_R_BW$results$standard_errors


error <- abs(test_fixed_R_BW$loglikf$hessian - hessian2) / test_fixed_R_BW$loglikf$hessian
sum(abs(error))

error <- abs(test_fixed_R_BW$loglikf$hessian - test_fixed_C_BW$loglikf$hessian) / test_fixed_R_BW$loglikf$hessian
sum(abs(error))














error <- abs(test_fixed_R_BW$loglikf$hessian - hessian2) / test_fixed_R_BW$loglikf$hessian
sum(abs(error))

error <- abs(test_fixed_R_BW$loglikf$hessian - test_fixed_C_BW$loglikf$hessian) / test_fixed_R_BW$loglikf$hessian
sum(abs(error))

##################################################################################################################


model_fixed <- modelGenerator(processedDCE, "fixed")
test_fixed_R_DCE <- runModel(model_fixed,  dev_mode = "R",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)
test_fixed_C_DCE <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)

error <- abs(test_fixed_R_DCE$loglikf$hessian - test_fixed_C_DCE$loglikf$hessian) / test_fixed_R_DCE$loglikf$hessian
error[error < 10^-3] <- 0
error

abs(test_fixed_C_DCE$LL - test_fixed_R_DCE$LL) / test_fixed_R_DCE$LL

standard_errors <- sqrt(diag(solve(test_fixed_R_DCE$loglikf$hessian)))
standard_errors

delta_grid <- suppressMessages(mvQuad::createNIGrid(dim = model_fixed$npp + model_fixed$nhop,
                                                    type = "GHN",
                                                    level = 3,
                                                    ndConstruction = "sparse"))

ghq_matrix1 <- as.matrix(cbind(delta_grid$weights, delta_grid$nodes))

hessian2 <- numDeriv::hessian(func = DCM:::llCalc_ghq_e,
                              x = test_fixed_C_DCE$loglikf$estimate,
                              model = model_fixed,
                              processed = model_fixed$data,
                              ghq_matrix1 = as.matrix(ghq_matrix1)
)

standard_errors2 <- sqrt(diag(solve(hessian2)))
standard_errors2
test_fixed_R_DCE$results$standard_errors
test_fixed_C_DCE$results$standard_errors

error <- abs(test_fixed_R_DCE$loglikf$hessian - hessian2) / test_fixed_R_DCE$loglikf$hessian
#error[error < 10^-4] <- 0
sum(abs(error))

error <- abs(test_fixed_R_DCE$loglikf$hessian - test_fixed_C_DCE$loglikf$hessian) / test_fixed_R_DCE$loglikf$hessian
#error[error < 10^-3] <- 0
sum(abs(error))

