#READ THE DATA IN
datamatrixfilename='sp.txt'
nmaxchoicesetsize=3
dataname='SP.RData'
source('jessydatain.R')


#SPECIFY FIXED COEFFICIENT MODEL
description='SP_FIXED'
dataname='SP.RData'
ndraws=100
ncovariates=13
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
save(file='SP_FIXED.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(14)
jessycheckmodel('SP_FIXED.RData',0)
jessycheckmodel('SP_FIXED.RData',1)
print(results)


#SPECIFY ONE FACTOR MODEL
description='SP_ONE FACTOR'
dataname='SP.RData'
ndraws=100
ncovariates=13
npp=ncovariates
nhop=1
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:13)*0+1),((1:npp)*0)) ,npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
delta[1,2]=-1
gamma   = matrix(c((1:13)*0+1)               ,npp        ,nhop)
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:26)*0+.1
print('Creating the matrices for the one factor model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)
save(file='SP_ONE FACTOR.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(14)
jessycheckmodel('SP_ONE FACTOR.RData',0)
jessycheckmodel('SP_ONE FACTOR.RData',1)
print(results)
