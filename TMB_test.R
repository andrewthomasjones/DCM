library(TMB)
library(DCM)

processedBW <- setUp(BWpriorities)
processedDCE <- setUp(DCEpriorities)

processedBW <- remove_variables(processedBW, "Accessibility_BW")

joined <- join_choicedatasets(processedBW, processedDCE)

model <- model_generator(joined , "fixed")
#res_fixed <- runModel(model)

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

model$epsilon[ ,1] <- res_fixed$results$estimate
model$epsilon[ ,2] <- 1e-8 #//FIXME

model$delta[ ,1] <- 0
model$delta[ ,2] <- 1

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
map <- list(
  gamma = model$gamma*NA, # the others will have to be filled via something like the current mapping function
  beta = model$beta*NA,
  phi = model$phi*NA,
  muepsilon = paste0("mu_epsilon_", seq_len(length(model$epsilon[, 1]))),
  mudelta = model$delta[, 1]*NA,
  logsigmaepsilon = log(matrix(model$epsilon[, 2], nrow = 1))*NA,
  logsigmadelta = log(matrix(model$delta[, 2], nrow = 1))*NA
)

map_f <- lapply(map, as.factor)

# system("rm TMB_code.o")
# system("rm TMB_code.so")


compile("TMB_code.cpp")
dyn.load(dynlib("TMB_code"))
set.seed(123)


# parameters$muepsilon[1] <- 0
# map$muepsilon[1] <- NA

obj <- MakeADFun(data, parameters, map = map_f, random = random,  DLL = "TMB_code", hessian = TRUE, inner.control = list(maxit = 1000))

obj$env$tracepar <- TRUE
## Test eval function and gradient
obj$fn(obj$par)
obj$gr(obj$par)

upper_lims = Inf
lower_lims = -Inf

## Fit model
opt <- nlminb(obj$par, obj$fn, obj$gr, upper = upper_lims, lower = lower_lims)
opt$par

rep <- sdreport(obj)
summary(rep, "random")
summary(rep, "fixed", p.value = TRUE)

res_fixed$results #FIXME orders seem different between methods but parameters the same
