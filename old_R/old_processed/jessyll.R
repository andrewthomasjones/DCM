jessyll<-function(workingvalues,model,conceptcode,nmaxchoicesetsize,datablock,ndecisionmakersblock,drawsmatrix){
source("jessyfrequencydistribution.R")
print("Jessyll__________________________________________________________________________")
print(model$description)
epsilonmatrix=model$epsilon
deltamatrix=model$delta
gammamatrix=model$gamma
deltamatrix=model$delta
betamatrix=model$beta
phimatrix=model$phi
npp=dim(epsilonmatrix)[1]
nhop=dim(deltamatrix)[1]
ndraws=dim(drawsmatrix)[1]
muepsilonparameters=(c(1:npp)*0)
mudeltaparameters=(c(1:nhop)*0)
sigmaepsilonparameters=(c(1:npp)*0)
sigmadeltaparameters=(c(1:nhop)*0)
gammaparameters=gammamatrix*0
betaparameters=betamatrix*0
phiparameters=phimatrix*0

i2<-0

for (i1 in 1:npp){

if (epsilonmatrix[i1,1]==1){
i2<-i2+1
muepsilonparameters[i1]<-workingvalues[i2]
}

if (epsilonmatrix[i1,1]==-1) muepsilonparameters[i1]<-1
}

for (i1 in 1:nhop){

if (deltamatrix[i1,1]==1){

i2<-i2+1
mudeltaparameters[i1]<-workingvalues[i2]
}

if (deltamatrix[i1,1]==-1)deltaepsilonparameters[i1]<-1
}

for (i1 in 1:npp){

if (epsilonmatrix[i1,2]==1){
i2<-i2+1
sigmaepsilonparameters[i1]<-abs(workingvalues[i2])
}

if (epsilonmatrix[i1,2]==-1)sigmaepsilonparameters[i1]<-1
}

for (i1 in 1:nhop){

if (deltamatrix[i1,2]==1){
i2<-i2+1
sigmadeltaparameters[i1]<-abs(workingvalues[i2])
}

if (deltamatrix[i1,2]==-1)sigmadeltaparameters[i1]<-1
}

for (i3 in 1: nhop){
for (i1 in 1:npp){
if (gammamatrix[i1,i3]==1){
i2<-i2+1
gammaparameters[i1,i3]<-workingvalues[i2]
}
if (gammamatrix[i1,i3]==-1)gammaparameters[i1,i3]<-1
}
}
for (i3 in 1:nhop){
for (i1 in 1:nhop){
if (betamatrix[i1,i3]==1){
i2<-i2+1
betaparameters[i1,i3]<-workingvalues[i2]
}
if (betamatrix[i1,i3]==-1)betaparameters[i1,i3]<-1
}
}
for (i1 in 1:(npp+nhop)) phiparameters[i1,i1]<- 1
for (i1 in 1:(npp+nhop-1)){
for (i3 in (i1+1):(npp+nhop)){
if (phimatrix[i1,i3]==1){
if (phimatrix[i3,i1]==1){
i2<-i2+1
phiparameters[i1,i3]=workingvalues[i2]
phiparameters[i3,i1]=workingvalues[i2]
}
}
}
}

print("Muepsilon parameters")
print(muepsilonparameters)
print("Mudelta parameters")
print(mudeltaparameters)
print("Sigmaepsilon parameters")
print(sigmaepsilonparameters)
print("Sigmadelta parameters")
print(sigmadeltaparameters)
print("Gamma parameters")
print(gammaparameters)
print("Beta parameters")
print(betaparameters)
print("Dimension Phi parameters")
print(dim(phiparameters))
print("Dimension draws")
print(dim(drawsmatrix))
drawsmatrix<-drawsmatrix%*%(phiparameters^0.5)
drawsepsilon=cbind(drawsmatrix[,1])
if (npp>1)  {for (i1 in 2:npp) drawsepsilon<-cbind(drawsepsilon,drawsmatrix[,i1])}
drawsdelta=cbind(drawsmatrix[,npp+1])
if (nhop>1) {for (i1 in 2:nhop) drawsdelta<-cbind(drawsdelta,drawsmatrix[,i1+npp])}
for (i1 in 1:ndraws) {
	for (i2 in 1:npp) drawsepsilon[i1,i2]=drawsepsilon[i1,i2]*sigmaepsilonparameters[i2]+muepsilonparameters[i2]
	for (i2 in 1:nhop) drawsdelta[i1,i2]=drawsdelta[i1,i2]*sigmadeltaparameters[i2]+mudeltaparameters[i2]
}
imatrix<-matrix((1:nhop*nhop)*0,nhop,nhop)
for (i1 in 1:nhop) imatrix[i1,i1]<-1
gb<-imatrix-betaparameters
gb<-solve(gb)
gb <- gammaparameters%*%gb
gb <- gb%*%t(drawsdelta)
gb<-gb+t(drawsepsilon)
conceptuse=conceptcode%*%model$code
gb<-conceptuse%*%gb
gb<-exp(gb)
pthisdm <- matrix((1:ndraws)*0+1,1,ndraws)
pthiscs <- matrix((1:ndraws)*0,1,ndraws)
print("ndecisionmakers in datablock")
print(ndecisionmakersblock)

ploglike <- 1:ndecisionmakersblock
i3 <- 1
iddm <- datablock[1,1]
bottom <- gb[1,]*0

dimdatablock <- dim(datablock)
print("Dimension of data block")
print(dimdatablock)
nlinesdatablock <- dimdatablock[1]
for (i1 in 1:nlinesdatablock) {
bottom <- bottom*0

for (i2 in 1:nmaxchoicesetsize) {
if (datablock[i1,i2+4]>0) bottom <- bottom+gb[datablock[i1,i2+4],]
}

pthiscs <- gb[datablock[i1,2],]/bottom
if (datablock[i1,1]==iddm) pthisdm <- pthisdm*pthiscs
if (datablock[i1,1]>iddm) {
ploglike[i3]=sum(pthisdm)/ndraws
i3 <- i3+1
pthisdm <- pthiscs
iddm <- datablock[i1,1]
}
}
ploglike[i3]=sum(pthisdm)/ndraws

ploglike <- log(ploglike)
loglike <- sum(ploglike)
loglike <- -loglike
print("Log likelihood")
print(loglike)
}
