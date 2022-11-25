filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP.txt'
nmaxchoicesetsize<-31
dataname<-'RP.RData'

datamatrix<-read_data(filename)

print(paste0('Dimensions of raw data: ', paste(dim(datamatrix), collapse = " ")))
print('Create concepts')

concept_list<-create_concepts(datamatrix, nmaxchoicesetsize)

fdd<-frequencydistribution(concept_list$data[,1])

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
