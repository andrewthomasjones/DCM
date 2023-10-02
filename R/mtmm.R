# else if (model_type == "mtmm") {
#
#   nhop <- ncovariates
#
#   nhop_d2_p2  <-  ceiling(nhop / 2) + 2
#   ncovariates_d2_p2 <- ceiling(ncovariates / 2) + 2
#   ncovariates_d2 <- ceiling(ncovariates / 2)
#
#   default_muep  <-  rep(1, ncovariates)
#   default_musig  <-  rep(0, ncovariates)
#   default_deltaep  <-  rep(0, nhop_d2_p2)
#   default_deltasig  <-  rep(-1, nhop_d2_p2)
#   default_gamma  <-  rbind(
#     cbind(diag(ncovariates_d2),
#           matrix(cbind(rep(1, ncovariates_d2), rep(0, ncovariates_d2)),
#                  ncol = 2)),
#     cbind(diag(ncovariates_d2),
#           matrix(cbind(rep(0, ncovariates_d2), rep(1, ncovariates_d2)),
#                  ncol = 2))
#   )
#
#   default_beta  <-  diag(nhop_d2_p2) * 0
#   default_muep_initial  <-  rep(.1, ncovariates)
#   default_musig_initial  <-  rep(NA, ncovariates)
#   default_deltaep_initial  <-  rep(NA, nhop)
#   default_deltasig_initial  <-  rep(NA, nhop)
#   default_gamma_initial  <-  rbind(cbind(diag(ncovariates_d2) * .1, matrix(cbind(rep(.1, ncovariates_d2), rep(NA, ncovariates_d2)), ncol = 2)),
#                                    cbind(diag(ncovariates_d2) * .1, matrix(cbind(rep(NA, ncovariates_d2), rep(.1, ncovariates_d2)), ncol = 2)))
#   default_beta_initial  <-  diag(nhop_d2_p2) * NA
#
#   epsilon_model  <-  matrix(cbind(default_muep, default_musig),
#                             ncol = 2,
#                             dimnames = list(attribute_names, c("mu", "sigma"))
#   )
#
#   delta_model  <-  matrix(cbind(default_deltaep, default_deltasig),
#                           ncol = 2,
#                           dimnames = list(paste("HoP_", 1:(nhop_d2_p2), sep = ""),
#                                           c("mu", "sigma"))
#   )
#
#   gamma_model  <-  matrix(default_gamma,
#                           ncol = ncovariates_d2_p2,
#                           dimnames = list(attribute_names,
#                                           paste("HoP_", 1:ncovariates_d2_p2, sep = ""))
#   )
#
#   beta_model  <-  matrix(default_beta,
#                          ncol = nhop_d2_p2,
#                          dimnames = list(paste("HoP_", 1:(nhop_d2_p2), sep = ""),
#                                          paste("HoP_", 1:(nhop_d2_p2), sep = ""))
#   )
#
#   epsilon_model_initial  <-  matrix(cbind(default_muep_initial, default_musig_initial),
#                                     ncol = 2,
#                                     dimnames = list(attribute_names, c("mu", "sigma"))
#   )
#
#   delta_model_initial  <-  matrix(cbind(default_deltaep_initial, default_deltasig_initial),
#                                   ncol = 2,
#                                   dimnames = list(paste("HoP_", 1:nhop, sep = ""),
#                                                   c("mu", "sigma"))
#   )
#
#   gamma_model_initial  <-  matrix(default_gamma_initial,
#                                   ncol = ncovariates_d2_p2,
#                                   dimnames = list(attribute_names,
#                                                   paste("HoP_", 1:ncovariates_d2_p2, sep = ""))
#   )
#
#   gamma_model_initial[gamma_model_initial == 0]  <-  NA
#   beta_model_initial  <-  matrix(default_beta_initial,
#                                  ncol = nhop_d2_p2,
#                                  dimnames = list(paste("HoP_", 1:(nhop_d2_p2), sep = ""),
#                                                  paste("HoP_", 1:(nhop_d2_p2), sep = ""))
#   )
