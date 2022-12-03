filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP.txt'
nmax_choiceset_size<-31
data_name<-'RP.RData'

data_matrix<-readData(filename)

print(paste0('Dimensions of raw data: ', paste(dim(data_matrix), collapse = " ")))
print('Create concepts')

concept_list<-createConcepts(data_matrix, nmax_choiceset_size)

fdd<-frequencyDistribution(concept_list)
fdd2<-frequencyDistribution2(concept_list)
##########################################################

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
