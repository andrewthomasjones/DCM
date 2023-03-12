jessyjoin <- function(data1,data2,datajoined){
source("jessyfrequencydistribution.R")
print('Joining two data files')
print(data1)
print(data2)
print('to form the joined data file')
print(datajoined)
load(data1)
data1=processed$data
concept1=processed$concept
ncovariates1=processed$ncovariates
nmaxchoicesetsize1=processed$nmaxchoicesetsize

load(data2)
data2=processed$data
concept2=processed$concept
ncovariates2=processed$ncovariates
nmaxchoicesetsize2=processed$nmaxchoicesetsize

nconcepts1=dim(concept1)[1]
nconcepts2=dim(concept2)[1]
ncovariates1=dim(concept1)[2]
ncovariates2=dim(concept2)[2]
nrowsdata1=dim(data1)[1]
nrowsdata2=dim(data2)[1]
ncolsdata1=dim(data1)[2]
ncolsdata2=dim(data2)[2]
ncolsdatam=max(ncolsdata1,ncolsdata2)
nmaxchoicesetsizem=max(nmaxchoicesetsize1,nmaxchoicesetsize2)
conceptm=matrix((1:((nconcepts1+nconcepts2)*(ncovariates1+ncovariates2)))*0,(nconcepts1+nconcepts2),(ncovariates1+ncovariates2))
datam=matrix((1:((nrowsdata1+nrowsdata2)*ncolsdatam))*0,(nrowsdata1+nrowsdata2),ncolsdatam)
dim(datam)
for (i1 in 1:nconcepts1){
for (i2 in 1:ncovariates1){
conceptm[i1,i2]=concept1[i1,i2]
}}
for (i1 in 1:nconcepts2){
for (i2 in 1:ncovariates2){
conceptm[i1+nconcepts1,i2+ncovariates1]=concept2[i1,i2]
}}

for (i1 in 1:nrowsdata1){
for (i2 in 1:ncolsdata1){
datam[i1,i2]=data1[i1,i2]
}
datam[i1,3]=1
}

for (i1 in 1:nrowsdata2){
datam[i1+nrowsdata1,1]=data2[i1,1]
datam[i1+nrowsdata1,2]=data2[i1,2]+nconcepts1
datam[i1+nrowsdata1,3]=2
datam[i1+nrowsdata1,4]=data2[i1,4]
for (i2 in 5:ncolsdata2){
datam[i1+nrowsdata1,i2]=data2[i1,i2]+nconcepts1
}}

datam=datam[order(datam[,1]),]

fddm=jessyfrequencydistribution(datam[,1])
ndecisionmakersm=dim(fddm)[1]
processed=list(datamatrix=c(0),data=datam,nmaxchoicesetsize=nmaxchoicesetsizem,ndecisionmakers=ndecisionmakersm,concept=conceptm,fdd=fddm)

print(processed)
print(data1)
print(data2)
print('to form the joined data file')
print(datam)

save(file=datajoined,list='processed')
datajoined
}
