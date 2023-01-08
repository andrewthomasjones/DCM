library(DCM)
library(tictoc)

filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP.txt'
nmax_choiceset_size<-31
data_name<-'RP.RData'

filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'
nmax_choiceset_size<-31
data_name<-'RP_SP.RData'

filename<-'/Users/uqajon14/Downloads/R Code_RP SP/SP.txt'
nmax_choiceset_size<-3
data_name<-'SP.RData'


data_matrix<-readData(filename)
# print(paste0('Dimensions of raw data: ', paste(dim(data_matrix), collapse = " ")))
# print('Create concepts')

tic()
concept_list<-createConcepts(data_matrix, nmax_choiceset_size)
fdd<-frequencyDistribution(concept_list)
b<-toc()

tic()
concept_list2<-createConcepts2(data_matrix, nmax_choiceset_size)
fdd2<-frequencyDistribution2(concept_list)
a<-toc()

as.numeric((b$toc-b$tic)/(a$toc-a$tic))
all(concept_list2$data == concept_list$data)
all(concept_list2$concept == concept_list$concept)

########################################################
concept_list<-createConcepts(data_matrix, nmax_choiceset_size) #83.084 sec elapsed
concept_list2<-createConcepts2(data_matrix, nmax_choiceset_size) #0.018 sec elapsed
########################################################

fdd<-frequencyDistribution(concept_list) #0.008 sec elapsed
fdd2<-frequencyDistribution2(concept_list) #0.001 sec elapsed
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

##########################################################

source('jessyfixed.R')
save(file=modelfixedname,list='model')
source('jessyglobal.R')
save(file=modelglobalname,list='model')
source('jessyrandom.R')
save(file=modelrandomname,list='model')
