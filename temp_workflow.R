#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)

tic()
library(DCM)

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
filename<-'/Users/uqajon14/Downloads/R Code_RP SP/SP.txt'

processed<-setUp(filename, header = F)

ncovariates<-processed$ncovariates
npp<-processed$ncovariates
nhop <-1

#set up default models
fixed_model <- model_generator(ncovariates, npp, nhop, 'Fixed Model', "Fixed")
random_model <- model_generator(ncovariates, npp, nhop, 'Random Model', "Random")
global_model <- model_generator(ncovariates, npp, nhop, 'Global Model', "Global")

#set up specific models
SP_fixed_model<-model_generator(ncovariates, npp, nhop, 'SP_FIXED', "Fixed")
SP_one_factor_model<-model_generator(ncovariates, npp, nhop, 'SP_ONE FACTOR', "Global")

runModel(SP_fixed_model, processed, 0, "RP_FIXED 0")
runModel(SP_fixed_model, processed, 1, "RP_FIXED 1")

runModel(SP_one_factor_model, processed, 0, "RP_1F 0")
runModel(SP_one_factor_model, processed, 1, "RP_1F 1")

toc()

