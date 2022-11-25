#READ THE DATA IN
datamatrixfilename='rp_SP.txt'
nmaxchoicesetsize=31
dataname='RP_SP.RData'
source('jessydatain.R')


#SPECIFY FIXED COEFFICIENT MODEL
description='RP_SP_FIXED'
dataname='RP_SP.RData'
ndraws=100
ncovariates=21
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
save(file='RP_SP_FIXED.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(22)
jessycheckmodel('RP_SP_FIXED.RData',0)
jessycheckmodel('RP_SP_FIXED.RData',1)
print(results)


#SPECIFY ONE FACTOR MODEL
description='RP_SP_ONE FACTOR'
dataname='RP_SP.RData'
ndraws=100
ncovariates=21
npp=ncovariates
nhop=1
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:21)*0+1),((1:npp)*0)),npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
delta[1,2]=-1
gamma   = matrix(c((1:21)*0+1)               ,npp        ,nhop)
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:42)*0+.1
print('Creating the matrices for the one factor model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)
save(file='RP_SP_ONE FACTOR.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(22)
jessycheckmodel('RP_SP_ONE FACTOR.RData',0)
jessycheckmodel('RP_SP_ONE FACTOR.RData',1)
print(results)


#SPECIFY MULTIFACTOR MODEL -- MULTITRAIT-MULTIMETHOD MODEL (MTMM)
description='RP_SP_MULTIFACTOR'
dataname='RP_SP.RData'
ndraws=100
ncovariates=21
npp=ncovariates
nhop=11
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:npp)*0+1),((1:npp)*0)),npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
delta[1,2]=-1
delta[2,2]=-1
delta[3,2]=-1
delta[4,2]=-1
delta[5,2]=-1
delta[6,2]=-1
delta[7,2]=-1
delta[8,2]=-1
delta[9,2]=-1
delta[10,2]=-1
delta[11,2]=-1
gamma   = matrix(c(1:npp*nhop)*0               ,npp        ,nhop)
gamma[1,1]=1
gamma[9,1]=1
gamma[2,2]=1
gamma[10,2]=1
gamma[3,3]=1
gamma[12,3]=1
gamma[4,4]=1
gamma[14,4]=1
gamma[5,5]=1
gamma[17,5]=1
gamma[6,6]=1
gamma[18,6]=1
gamma[7,7]=1
gamma[19,7]=1
gamma[8,8]=1
gamma[20,8]=1
gamma[1,9]=1
gamma[2,9]=1
gamma[3,9]=1
gamma[4,9]=1
gamma[5,9]=1
gamma[6,9]=1
gamma[7,9]=1
gamma[8,9]=1
gamma[9,10]=1
gamma[10,10]=1
gamma[11,10]=1
gamma[12,10]=1
gamma[13,10]=1
gamma[14,10]=1
gamma[15,10]=1
gamma[16,10]=1
gamma[17,10]=1
gamma[18,10]=1
gamma[19,10]=1
gamma[20,10]=1
gamma[21,10]=1
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
beta[1,11]=1
beta[2,11]=1
beta[3,11]=1
beta[4,11]=1
beta[5,11]=1
beta[6,11]=1
beta[7,11]=1
beta[8,11]=1
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:66)*0+.1
print('Creating the matrices for the multifactor model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)
save(file='RP_SP_MULTIFACTOR.RData',model)
source('jessycheckmodel.R')
source('jessydraws.R')
jessydraws(32)
jessycheckmodel('RP_SP_MULTIFACTOR.RData',0)
jessycheckmodel('RP_SP_MULTIFACTOR.RData',1)
print(results)

