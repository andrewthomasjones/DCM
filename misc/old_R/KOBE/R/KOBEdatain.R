# KOBEdatain reads in the data from an Excel spreadsheet and prompts the user for information

suppressMessages(suppressWarnings(if (!require("pacman")) install.packages("pacman")))
suppressMessages(suppressWarnings(pacman::p_load(svDialogs,readxl,openxlsx)))
require('tools')

datamatrixfilenames=paste("./DATA/",select.list(list.files(path = "./DATA", pattern = ".csv"), multiple = TRUE,
                                                title = "Read in data (hold CTRL to select multiple)", graphics = TRUE),sep="")

filenames <- as.matrix(substr(file_path_sans_ext(datamatrixfilenames),8,999))
filenames


cat(paste("Reading in", length(filenames), "dataset(s) in alphabetical order... please wait a moment (or a while)", "\n"))

for (i in 1:nrow(filenames)){

datamatrixfilename=paste('./DATA/',filenames[i],'.csv',sep="")
dataname=filenames[i]

datamatrix <- read.csv(datamatrixfilename, header = TRUE)
nmaxchoicesetsize = as.numeric(max(unlist(rle(datamatrix[,2])[1])))

source('./R/KOBEfrequencydistribution.R')
source('./R/KOBEcreateconcepts.R')
fdd=KOBEfrequencydistribution(data[,1])
ndecisionmakers=dim(fdd)[1]
ncovariates=dim(concept)[2]
lcovariates=c(1:ncovariates)*0
for (i1 in 1:ncovariates) {lcovariates[i1]=paste('Cov',i1)}
dlength=nrow(datamatrix)
attributenames=colnames(datamatrix[4:ncol(datamatrix)])
processed=list(datamatrix=datamatrix,dlength=dlength,datamatrixfilename=datamatrixfilename,attributenames=attributenames,data=data,ncovariates=ncovariates,nmaxchoicesetsize=nmaxchoicesetsize,ndecisionmakers=ndecisionmakers,concept=concept,fdd=fdd)
save(file=paste('./DATA/',filenames[i],".RData",sep=""),list='processed')
cat(paste("\n",filenames[i],".RData"," saved in ",getwd(),"/DATA/", "\n",sep=""))

source('./R/KOBEfixedemi.r')
source('./R/KOBErandomemi.r')
source('./R/KOBE1facemi.r')

if(i<length(filenames)){
  cat("\n Reading in the next dataset...")
}
}

cat(paste("\n Finished reading in datasets","\n Number of datasets read in =",length(filenames)))
