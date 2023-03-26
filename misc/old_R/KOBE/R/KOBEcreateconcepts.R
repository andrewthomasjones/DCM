## KOBEcreateconcepts sets up the matrices for estimation... more details required.

dimdatamatrix=dim(datamatrix)
source("./R/KOBEfrequencydistribution.R")
nlinesdatamatrix <- dimdatamatrix[1]
ncovariates <- dimdatamatrix[2]-3

iddm <- datamatrix[1,1]
idcs <- datamatrix[1,2]
databig <- matrix((1:nlinesdatamatrix*(nmaxchoicesetsize+4))*0,nlinesdatamatrix,nmaxchoicesetsize+4)
databig[1,1]=iddm
if (datamatrix[1,3]==1) databig[1,2]<- 1
databig[1,4] <- 1
conceptbig <- matrix((1:nlinesdatamatrix*ncovariates)*0,nlinesdatamatrix,ncovariates)
for (i3 in 1:ncovariates) conceptbig[1,i3] <- datamatrix[1,i3+3]
databig[1,5] <-1

i1 <- 1
i2 <- 1
i5 <- 1
i6 <- 1

# i1 is the count of lines moving through the data matrix
# i2 is the build of the number of rows in databig
# i3 is the count of covariates moving through the row of the data matrix
# i4 is the count of lines moving through the concept matrix
# i5 is the build of the number of concepts in a row in databig
# i6 is build of the number of concepts

while (i1<nlinesdatamatrix){
i1 <- i1+1
matchnumber <- 0
i4 <- 0

while (i4<i6){
i4 <- i4+1
matchcount <-0

for (i3 in 1:ncovariates) {
if (conceptbig[i4,i3]==datamatrix[i1,i3+3]) matchcount <- matchcount+1
}

if (matchcount==ncovariates) {
matchnumber <- i4
i4 <- i6
}
}

if (matchnumber==0) {
i6 <- i6+1
matchnumber <- i6
for (i3 in 1:ncovariates) conceptbig[i6,i3] <- datamatrix[i1,i3+3]
}


if (datamatrix[i1,1]==iddm) {

if (datamatrix[i1,2]==idcs) {
i5 <- i5+1
databig[i2,i5+4] <- matchnumber
if (datamatrix[i1,3]==1) databig[i2,2] <- matchnumber
}
}

if (datamatrix[i1,1]==iddm) {

if (datamatrix[i1,2]>idcs) {

idcs <- datamatrix[i1,2]
i5 <- 1
i2 <- i2+1
databig[i2,1] <- iddm
databig[i2,4] <- 1
databig[i2,i5+4] <- matchnumber
if (datamatrix[i1,3]==1) databig[i2,2] <- matchnumber
}
}

if (datamatrix[i1,1]>iddm) {
iddm <- datamatrix[i1,1]
idcs <- datamatrix[i1,2]
i5 <- 1
i2 <- i2+1
databig[i2,1] <- iddm
databig[i2,4] <- 1
databig[i2,i5+4] <- matchnumber
if (datamatrix[i1,3]==1) databig[i2,2] <- matchnumber

}


}

concept <- matrix((1:i6*ncovariates)*0,i6,ncovariates)
for (i8 in 1:i6) {
for (i9 in 1:ncovariates) {
concept[i8,i9]=conceptbig[i8,i9]
}
}
nconcepts <- i6

data <- matrix((1:i2*(nmaxchoicesetsize+4))*0,i2,nmaxchoicesetsize+4)
for (i8 in 1:i2) {
for (i9 in 1:(nmaxchoicesetsize+4)) {
data[i8,i9]=databig[i8,i9]
}
}
nlinesdata <- i2

fdd <- KOBEfrequencydistribution(data[,1])

