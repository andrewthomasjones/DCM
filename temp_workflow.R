#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
library(tictoc)
tic()


library(DCM)

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'

filename<-'./RP_SP.txt'

processed<-setUp(filename, header = T)

nhop<-1

#set up default models
fixed_model <- model_generator(processed, nhop, 'Fixed Model', "Fixed")
random_model <- model_generator(processed, nhop, 'Random Model', "Random")
global_model <- model_generator(processed, nhop, 'Global Model', "Global")

#set up specific models
fixed_model<-model_generator(processed, nhop, 'FIXED', "Fixed")
one_factor_model<-model_generator(processed, nhop, 'ONE FACTOR', "Global")


r1<-runModel(one_factor_model, "n 1000 shuf", ndraws=1000, shuffle=TRUE)
r2<-runModel(one_factor_model, "n 1000 noshuf", ndraws=1000, shuffle=FALSE)




toc() #118.491 sec elapsed
