#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
library(tictoc)
tic()
library(DCM)

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
#filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'

filename<-'./RP_SP.txt'

processed<-setUp(filename, header = T)

ncovariates<-processed$ncovariates
npp<-processed$ncovariates
nhop<-1

#set up default models
fixed_model <- model_generator(ncovariates, npp, nhop, 'Fixed Model', "Fixed")
random_model <- model_generator(ncovariates, npp, nhop, 'Random Model', "Random")
global_model <- model_generator(ncovariates, npp, nhop, 'Global Model', "Global")

#set up specific models
fixed_model<-model_generator(ncovariates, npp, nhop, 'FIXED', "Fixed")
one_factor_model<-model_generator(ncovariates, npp, nhop, 'ONE FACTOR', "Global")

#runModel(fixed_model, processed, 0, "FIXED 0")
#runModel(fixed_model, processed, 1, "FIXED 1")

runModel(one_factor_model, processed, 0, "1F 0")
runModel(one_factor_model, processed, 1, "1F 1")

toc() #118.491 sec elapsed
