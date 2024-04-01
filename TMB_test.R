library(TMB)
library(DCM)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBW <- remove_variables(processedBW, "Accessibility_BW")

joined <- join_choicedatasets(processedBW, processedDCE)

model <- model_generator(processedDCE, "random")
res_fixed <- runModel(model)


#model <- model_generator(joined, "mtmm")


choice_picker <- function(data){
  choices <- data[,2]
  slots <- data[, 5:ncol(data)]
  locations <-  unlist(mapply(function(x, a){which(slots[x, ] == a)[1]}, seq_len(nrow(slots)), choices)) #why do some rows in data matrix have repeats??

  d <- slots*0
  for(i in seq_len(nrow(slots))){
    d[i, locations[i]] <- 1
  }


  return(d)
}



data <- list(concept = model$data$concept,
             data = model$data$data[, 5:ncol(model$data$data)],
             code = model$code,
             group = factor(model$data$data[,1]),
             choices = choice_picker(model$data$data), #matrix of choices - each row all zeros but has one 1
             imatrix = diag(model$nhop)
             )

model$epsilon[ ,1] <- 0.1#res_fixed$results$estimate
model$epsilon[ ,2] <- 1e-8 #e-12 #//FIXME has to be very small for fixed

model$delta[ ,1] <- 0
model$delta[ ,2] <- 1

model$gamma[model$gamma==-1]<-1

parameters <- list(
                  gamma = model$gamma,
                  beta = model$beta,
                  phi = model$phi,
                  muepsilon = matrix(model$epsilon[, 1], nrow = 1),
                  mudelta = matrix(model$delta[, 1], nrow = 1),
                  logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1)),
                  logsigmadelta = log(matrix(model$delta[, 2], nrow = 1)),
                  epsilon = matrix(rnorm(model$data$ndecisionmakers*model$npp), nrow = model$data$ndecisionmakers, ncol = model$npp, byrow=TRUE),
                  delta = matrix(rnorm(model$data$ndecisionmakers*model$nhop), nrow = model$data$ndecisionmakers, ncol = model$nhop, byrow=TRUE)
)

random <- c("epsilon", "delta")
#3913.522
#3822.771

#fixed model
gamma1<- model$gamma
beta1<- model$beta

gamma1[model$gamma == 0] <- NA
gamma1[!is.na(gamma1)] <- paste0("gamma_", seq_len(sum(!is.na(gamma1))))

beta1[model$beta==0] <- NA
beta1[!is.na(beta1)] <- paste0("beta_", seq_len(sum(!is.na(beta1))))

map <- list(
  gamma =   gamma1, #paste0("gamma_", seq_len(length(model$gamma))), #model$gamma*NA, # the others will have to be filled via something like the current mapping function
  beta = beta1, #model$beta*NA,
  phi = model$phi*NA,
  muepsilon = paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1]))),
  mudelta = model$delta[, 1]*NA,
  logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1))*NA, #paste0("logsigma_epsilon_", seq_len(length(model$epsilon[, 1]))),# log(matrix(model$epsilon[, 2], nrow = 1))*NA,
  logsigmadelta = log(matrix(model$delta[, 2], nrow = 1))*NA
)

map_f <- lapply(map, as.factor)

# system("rm TMB_code.o")
# system("rm TMB_code.so")


compile("TMB_code.cpp")
dyn.load(dynlib("TMB_code"))
obj <- MakeADFun(data, parameters, map = map_f, random = random,  DLL = "TMB_code", hessian = TRUE)


# parameters$muepsilon[1] <- 0
# map$muepsilon[1] <- NA



obj$env$tracepar <- TRUE
## Test eval function and gradient
obj$fn(obj$par)
obj$gr(obj$par)

upper_lims = Inf
lower_lims = -Inf

## Fit model
opt <- nlminb(obj$par, obj$fn, obj$gr, upper = upper_lims, lower = lower_lims)
opt$par

rep <- sdreport(obj, bias.correct = TRUE)
summary(rep, "random")
summary(rep, "fixed", p.value = TRUE)
#
model <- model_generator(joined, "mtmm")
res_fixed <- runModel(model, verbose = 2)
res_fixed$results #FIXME orders seem different between methods but parameters the same
#12688.9436092967
