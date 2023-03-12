#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
library(tictoc)

library(DCM)

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'

processed<-setUp(filename, header = F)

nhop<-1

#set up default models
fixed_model <- model_generator(processed, nhop, 'Fixed Model', "Fixed")
random_model <- model_generator(processed, nhop, 'Random Model', "Random")
global_model <- model_generator(processed, nhop, 'Global Model', "Global")

#set up specific models
fixed_model<-model_generator(processed, nhop, 'FIXED', "Fixed")
one_factor_model<-model_generator(processed, nhop, 'ONE FACTOR', "Global")


r1<-runModel(one_factor_model, "n 100 1F", ndraws=100)
r2<-runModel(fixed_model, "n 100 fixed", ndraws=100)
r3<-runModel(random_model, "n 100 Rand", ndraws=100)
r4<-runModel(global_model, "n 100 Glob", ndraws=100)

summariseModelList(list(r1,r2,r3, r4))

parPrintOld(r1)
parPrintOld(r2)
parPrintOld(r3)
parPrintOld(r4)




