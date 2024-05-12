
sd_CI <- function(s, SE, upper = FALSE, alpha = 0.05){

  k <-  s^2 / SE^2

  crit1 <- qchisq(1 - alpha / 2, k)
  crit2 <- qchisq(alpha / 2, k)

  if(upper == TRUE){
    res <- sqrt(k / crit2) * s
  }else{
    res <- sqrt(k / crit1) * s
  }

  return(res)
}

n_CI <- function(x, SE, upper = FALSE, alpha = 0.05){

  crit1 <- qnorm(alpha / 2)
  crit2 <- qnorm(1 - alpha / 2)

  if(upper == TRUE){
    res <- x + SE * crit2
  }else{
    res <- x + SE * crit1
  }

  return(res)
}

CI_results <- function(results, alpha = 0.05){

  results$lower <- NA
  results$upper <- NA
  results$variance <- case_when(str_detect(results$parameters, "_sig_") ~ TRUE,
                                TRUE ~ FALSE)

  temp1 <- results %>% filter(variance) %>% mutate(lower = sd_CI(estimate, standard_errors, FALSE), upper = sd_CI(estimate, standard_errors, TRUE))
  temp2 <- results %>% filter(!variance) %>% mutate(lower = n_CI(estimate, standard_errors, FALSE), upper = n_CI(estimate, standard_errors, TRUE))

  results_CI <- rbind(temp2, temp1)[, 1:5]

  return(results_CI)
}

library(DCM)

processedDCE <- setUp(DCEpriorities)
model <- model_generator(processedDCE, "random")

res <- runModel(model, dev_mode = "TMB")
res2 <- runModel(model)
res3 <- runModel(model, dev_mode = "Cdraws")

CI_results(res$results)
#CI_results(res2$results[, 1:3])
CI_results(res3$results[, 1:3])





