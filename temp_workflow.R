#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)

library(DCM)
library(tictoc)

filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP.txt'
nmax_choiceset_size<-31
data_name<-'Test.RData'

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
#nmax_choiceset_size<-80
#data_name<-'Test.RData'

data_matrix<-readData(filename)

concept_list<-createConcepts2(data_matrix, nmax_choiceset_size) #0.018 sec elapsed

fdd<-frequencyDistribution2(concept_list) #0.001 sec elapsed


################################# remainder of jessydatain ####################################################
ndecisionmakers<-dim(fdd)[1]
lcovariates<-array(0, concept_list$ncovariates)
for (i1 in 1:concept_list$ncovariates) {
  lcovariates[i1]=paste('Cov',i1)
}

processed<-list(data_matrix=data_matrix,
               data_name=data_name,
               data=concept_list$data,
               ncovariates=ncovariates,
               nmax_choiceset_size=nmax_choiceset_size,
               ndecisionmakers=ndecisionmakers,
               concept=concept_list$concept,
               lcovariates=lcovariates,
               fdd=fdd)

fixed_model <- model_generator(ncovariates, ncovariates, nhop, 'Fixed Model', "Fixed", 100)
random_model <- model_generator(ncovariates, ncovariates, nhop, 'Random Model', "Random", 100)
global_model <- model_generator(ncovariates, ncovariates, nhop, 'Global Model', "Global", 100)

RP_fixed_model <-model_generator(ncovariates, npp, nhop, 'RP_FIXED', "Fixed", 100)
RP_one_factor_model <-model_generator(ncovariates, npp, nhop, 'RP_ONE FACTOR', "Global", ndraws)
############################################################################################

source('jessycheckmodel.R')

draw_store<-draws(9)

jessycheckmodel('RP_FIXED.RData',0)
jessycheckmodel('RP_FIXED.RData',1)

draw_store<-draws(9)

jessycheckmodel('RP_ONE FACTOR.RData',0)
jessycheckmodel('RP_ONE FACTOR.RData',1)



