KOBEjoin <- function(data1,data2,KOBEjoined){
  source("./R/KOBEfrequencydistribution.R")
  load(data1)
  data1=processed$data
  concept1=processed$concept
  ncovariates1=processed$ncovariates
  nmaxchoicesetsize1=processed$nmaxchoicesetsize
  attributenames1=processed$attributenames
  dlength1=nrow(processed$datamatrix)
# headers1=processed$attributenames
    
  load(data2)
  data2=processed$data
  concept2=processed$concept
  ncovariates2=processed$ncovariates
  nmaxchoicesetsize2=processed$nmaxchoicesetsize
  attributenames2=processed$attributenames
  dlength2=nrow(processed$datamatrix)
# headers2=processed$attributenames
  
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
  dlength=dlength1+dlength2
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
  
  fddm=KOBEfrequencydistribution(datam[,1])
  ndecisionmakersm=dim(fddm)[1]
  
  
  
  processed=list(attributenames=c(attributenames1,attributenames2),data=datam,dlength=dlength,nmaxchoicesetsize=nmaxchoicesetsizem,ncovariates=ncovariates1+ncovariates2,ndecisionmakers=ndecisionmakersm,concept=conceptm,fdd=fddm)
  
  save(file=datajoined,list='processed')
  datajoined
}

datafiles=paste("./DATA/",select.list(list.files(path = "./DATA", pattern = ".RData"), multiple = TRUE,
                                               title = "Select TWO datasets to join", graphics = TRUE),sep="")

data1=datafiles[1]

data2=datafiles[2]

datajoined=paste0('./DATA/',
                  substr(data1, start = 8, stop = nchar(data1)-6),
                  substr(data2, start = 8, stop = nchar(data2)-6),'.RData')

dataname=substr(datajoined,start = 8, stop = nchar(datajoined)-6)

KOBEjoin(data1,data2,KOBEjoined)

cat("Data successfully combined \n")
cat(paste(substr(data1,8,999)," and ",substr(data2,8,999)," have been combined and saved as\ ",substr(datajoined,8,999)," \n in ",getwd(),"/DATA",sep = ""))

load(datajoined)
ncovariates=processed$ncovariates
attributenames=processed$attributenames

source('./R/KOBEfixedemi.r')
source('./R/KOBErandomemi.r')
source('./R/KOBE1facemi.r')
# source('./R/KOBEmtmmemi.r') not working yet, for some reason the gamma matrix cannot be built, there's a problem in the coding somewhere
