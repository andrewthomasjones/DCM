library(DCM)
library(tictoc)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)
joined <- join_choicedatasets(processedBW, processedDCE)

model_1f <- model_generator(joined, "one-factor")
model_fixed <- model_generator(processedDCE, "fixed")
model_random <- model_generator(processedDCE, "random")
model_mtmm <- model_generator(joined, "mtmm")

res_1f <- runModel(model_1f)
res_fixed <- runModel(model_fixed)
res_random <- runModel(model_random)
res_mtmm  <- runModel(model_mtmm)

round(res_1f$results$estimate,1)
round(res_fixed$results$estimate,1)
round(res_random$results$estimate,1)
round(res_mtmm$results$estimate,1)

processed <- remove_variables(processed, "Accessibility_BW" )
model<- model_generator(processed, "fixed")
res <- runModel(model)
chosen_values <- res$results$estimate



s1 <- scores(chosen_values,  model,  processed)

a <- t(s1) %*% s1

b <- solve(res$loglikf$hessian) %*% a %*% solve(res$loglikf$hessian)
sqrt(diag(b))
sqrt(diag(solve(res$loglikf$hessian)))


processedDCE <- setUp(DCEpriorities)
processedBW <- setUp(BWpriorities)

#joined <- join_choicedatasets(processedBWr , processedDCE)

gt <- 1e-6
st <- 1e-6

model_fixed <- model_generator(processedDCE, "fixed")
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








model_fixed <- model_generator(processedBW, "fixed")
test_fixed_R_BW <- runModel(model_fixed,  dev_mode = "R",  ghq_size = 3, verbose = 0, gradtol = gt, steptol = st)
test_fixed_R_BW$results

model_fixed <- model_generator(processedDCE, "fixed")
test_fixed_R_DCE <- runModel(model_fixed,  dev_mode = "C",  ghq_size = 3, verbose = 2, gradtol = gt, steptol = st)

model_fixed <- model_generator(joined, "fixed")
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


model_fixed <- model_generator(processedDCE, "fixed")
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

