npp=ncovariates
nhop=1
ndraws=100
description='Global Model'
code    = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon = matrix(c(((1:npp)*0+1),((1:npp)*0)),npp        ,2)
delta   = matrix(c(1:nhop*2)*0               ,nhop       ,2)
delta[1,2]=-1
gamma   = matrix(c(1:npp*nhop)*0             ,npp        ,nhop)
for (i1 in 1:(npp)) gamma[i1,1] = 1
beta    = matrix(c(1:nhop*nhop)*0            ,nhop       ,nhop)
phi     = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }
initialvalues=c(1:(npp+npp))*0+.1
print('Creating the matrices for the global model')
print(description)
model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
print(model)