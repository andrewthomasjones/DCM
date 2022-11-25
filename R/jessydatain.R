datamatrix <- read.table(datamatrixfilename)
print('Dimensions of raw data')
print(dim(datamatrix))
source('jessyfrequencydistribution.R')
print('Create concepts')
source('jessycreateconcepts.R')
fdd=jessyfrequencydistribution(data[,1])
ndecisionmakers=dim(fdd)[1]
ncovariates=dim(concept)[2]
lcovariates=c(1:ncovariates)*0
for (i1 in 1:ncovariates) {lcovariates[i1]=paste('Cov',i1)}
print(lcovariates)
processed=list(datamatrix=datamatrix,datamatrixfilename=datamatrixfilename,data=data,ncovariates=ncovariates,nmaxchoicesetsize=nmaxchoicesetsize,ndecisionmakers=ndecisionmakers,concept=concept,fdd=fdd)
print(processed)
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


