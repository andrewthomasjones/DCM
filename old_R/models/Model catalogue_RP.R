#READ THE DATA IN
datamatrixfilename='rp.txt'
nmaxchoicesetsize=31
dataname='RP.RData'
source('jessydatain.R')


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
