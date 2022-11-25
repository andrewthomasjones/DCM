jessysimulate <- function(modelname,specifiedvalues){
source("jessyll.R")
load(modelname)
load(model$dataname)
npp=dim(model$epsilon)[1]
nhop=dim(model$delta)[1]
nrc=npp+nhop
nmaxchoicesetsize=processed$nmaxchoicesetsize
nlinesdata=dim(processed$data)[1]
ndecisionmakers=processed$ndecisionmakers

i4 <- 0
drawsmatrix <- matrix((1:nrc)*0,1,nrc)
datablock <- matrix(((1:(4+nmaxchoicesetsize))*0),1,(4+nmaxchoicesetsize))
camansdualsimallprob <- matrix((1:(nlinesdata*(nmaxchoicesetsize+1+1+1+1)))*0,nlinesdata,(nmaxchoicesetsize+1+1+1+1))
simdata <- matrix((1:(nlinesdata*(nmaxchoicesetsize+4)))*0,nlinesdata,(nmaxchoicesetsize+4))
ndraws <- 1
ndecisionmakersblock <- 1
nlinesdatablock <- 1


for (i1 in 1:ndecisionmakers) {
for (i2 in 1:nrc) {
drawsmatrix[1,i2] <- rnorm(1)
}

print("decisionmaker   i1")
print(i1)
print("drawsmatrix")
print(drawsmatrix)
for (i3 in 1:processed$fdd[i1,2]) {
i4 <- i4+1
datablock[1,]=processed$data[i4,]
sumofprob <- 0
simprobcutpoint <- runif(1)
simchoice <- nmaxchoicesetsize
choicemade <- 0

for (i6 in 1:nmaxchoicesetsize) {
datablock[1,2] <- datablock[1,i6+4]
camansdualsim=jessyll(specifiedvalues,model,processed$concept,nmaxchoicesetsize,datablock,1,drawsmatrix)
print("decision i3     and choice sim i6     camansdualsim and probability camansdualsimprob")
print(i3)
print(i6)
print(camansdualsim)
camansdualsimprob <- exp(-camansdualsim)
print(camansdualsimprob)
print(simprobcutpoint)
print(datablock)
print("Just printed data block - not simulated choice")
camansdualsimallprob[i4,1] <- i1
camansdualsimallprob[i4,i6+1] <- camansdualsimprob
sumofprob <- sumofprob+camansdualsimprob

if (sumofprob > simprobcutpoint) {
if (choicemade < 1) {
choicemade <- 1
simchoice <- i6
}}

}
camansdualsimallprob[i4,nmaxchoicesetsize+1+1] <- sumofprob
camansdualsimallprob[i4,nmaxchoicesetsize+1+1+1] <- simprobcutpoint

camansdualsimallprob[i4,nmaxchoicesetsize+1+1+1+1] <- simchoice
simdata[i4,1] <- processed$data[i4,1]
simdata[i4,2] <- processed$data[i4,simchoice+4]
simdata[i4,3] <- processed$data[i4,3]
simdata[i4,4] <- processed$data[i4,4]
for (i7 in 1:nmaxchoicesetsize) {
simdata[i4,i7+4] <- processed$data[i4,i7+4]
}
print(simdata[i4,])
print("Just printed simulated data")
}
}
print("Now to save the simulated data")
simdatestamp=date()
processedt=list(datamatrix=c(0),data=simdata,nmaxchoicesetsize=processed$nmaxchoicesetsize,ndecisionmakers=processed$ndecisionmakers,concept=processed$concept,fdd=processed$fdd,simprobs=camansdualsimallprob,simmodel=model,simmodelname=modelname,simdatestamp=simdatestamp,simspecifiedvalues=specifiedvalues)
processed=processedt
print('Name of the model used in the simulation')
print(modelname)
simdataname=paste('Datasim',modelname)
print('Name of simulated data file')
print(simdataname)
print('Hi 1')
save(file=simdataname,list='processed')
print('Hi 2')
print('Name of simulated data workspace')
print(simdataname)
model$dataname=simdataname
simmodelname=paste('modsim',modelname)
save(file=simmodelname,list='model')
print('Name of the model for the simulated data')
print(simmodelname)
}