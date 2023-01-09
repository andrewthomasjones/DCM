#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)

#####################################################################################
#covers lines 1-4 of model catalog files lines 1-7 of jessydatain

library(DCM)
#filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP.txt'
filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
nmax_choiceset_size<-20
data_name<-'Test.RData'

data_matrix<-readData(filename)

print(paste0('Dimensions of raw data: ', paste(dim(data_matrix), collapse = " ")))
print('Create concepts')

########################################################
concept_list<-createConcepts(data_matrix, nmax_choiceset_size) #83.084 sec elapsed
concept_list2<-createConcepts2(data_matrix, nmax_choiceset_size) #0.018 sec elapsed
########################################################

fdd<-frequencyDistribution(concept_list) #0.008 sec elapsed
fdd2<-frequencyDistribution2(concept_list) #0.001 sec elapsed
##########################################################

################################# remainder of jessydatain ####################################################
ndecisionmakers<-dim(fdd)[1]
ncovariates<-dim(concept)[2]
lcovariates<-array(0, ncovariates)
for (i1 in 1:ncovariates) {lcovariates[i1]=paste('Cov',i1)}
#print(lcovariates)

processed=list(datamatrix=datamatrix,datamatrixfilename=datamatrixfilename,data=data,ncovariates=ncovariates,nmaxchoicesetsize=nmaxchoicesetsize,ndecisionmakers=ndecisionmakers,concept=concept,fdd=fdd)
#print(processed)
save(file=dataname,list='processed')
print('Dimensions of raw datamatrix')
print(dim(datamatrix))
print('Dimensions of process data: should be ndecisionmakers by nmaxchoicesetsize+4')
print(dim(data))
print('Dimensions of concept matrix')
print(dim(concept))
source('jessyfixed.R')
save(file=modelfixedname,list='model')
source('jessyglobal.R')
save(file=modelglobalname,list='model')
source('jessyrandom.R')
save(file=modelrandomname,list='model')

############################################################################################

#SPECIFY FIXED COEFFICIENT MODEL
description='RP_FIXED'
dataname='RP.RData'
ndraws=100
ncovariates=8
npp=ncovariates
nhop=1
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:npp)*0+1),((1:npp)*0)),npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
gamma   = matrix(c(1:npp*nhop)*0             ,npp        ,nhop)
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:npp)*0+.1
print('Creating the matrices for the fixed coefficient model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)
save(file='RP_FIXED.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(9)
jessycheckmodel('RP_FIXED.RData',0)
jessycheckmodel('RP_FIXED.RData',1)
print(results)

#SPECIFY ONE FACTOR MODEL
description='RP_ONE FACTOR'
dataname='RP.RData'
ndraws=100
ncovariates=8
npp=ncovariates
nhop=1
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:8)*0+1),((1:npp)*0)),npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
delta[1,2]=-1
gamma   = matrix(c((1:8)*0+1)               ,npp        ,nhop)
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:16)*0+.1
print('Creating the matrices for the one factor model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)
save(file='RP_ONE FACTOR.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(9)
jessycheckmodel('RP_ONE FACTOR.RData',0)
jessycheckmodel('RP_ONE FACTOR.RData',1)
print(results)


