library(TMB)
compile("TMB_code.cpp")
dyn.load(dynlib("TMB_code"))
set.seed(123)


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



data <- list(concept = model$concept,
             data = model$data,
             code = model$code,
             group = factor(model$data[,1]),
             choices = choice_picker(model$data), #matrix of choices - each row all zeros but has one 1
             nmax_choiceset_size=model$nmax_choiceset_size,
             ndecisionmakers = model$ndecisionmakers,
             npp=model$npp,
             nhop=model$nhop)



# // options <- options[options>0]
# // choice <-  which(options == data[i, 2])
# // d_i <- rep(0, length(probs))
# // d_i[choice] <- 1

parameters <- list(gamma = gamma,
                  beta = beta,
                  phi = phi,
                  muepsilon = muepsilon,
                  mudelta = mudelta,
                  sigmaepsilon = sigmaepsilon,
                  sigmadelta = sigmadelta,
                  epsilon = epsilon,
                  delta = delta
)

random <- list(epsilon = epsilon,
               delta = delta
)


map <- list()
  # Optionally, a simple mechanism for collecting and fixing parameters from R is available through the map argument. A map is a named list of factors with the following properties:
  #
  #   names(map) is a subset of names(parameters).
  #
  # For a parameter "p" length(map$p) equals length(parameters$p).
  #
  # Parameter entries with NAs in the factor are fixed.
  #
  # Parameter entries with equal factor level are collected to a common value.
  #


obj <- MakeADFun(data, parameters, map = map, random = random, DLL = "TMB_code", hessian = TRUE)

## Test eval function and gradient
obj$fn()
obj$gr()

upper_lims = Inf
lower_lims = -Inf

## Fit model
opt <- nlminb(obj$par, obj$fn, obj$gr, upper = upper_lims, lower = lower_lims)
opt$par
