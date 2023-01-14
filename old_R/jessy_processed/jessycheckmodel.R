jessycheckmodel <- function(modelname,runoption){
print('Start of printing from jessycheckmodel.R source')
source('jessyll.R')
source('jessyllmax.R')
source('jessyparcount.R')
print('Modelname')
print(modelname)
load(modelname)
print('Print model in full')
print(model)
parcount<-jessyparcount(model)
print('Number of parameters: total epsilonmu, epsilonsig, deltamu, deltasig, gamma, beta ,phi')
print(parcount)
load(model$dataname)
print('Specific details of the data and model')
print("Dimension of datamatrix")
print(dim(processed$datamatrix))
print("Maximun choice set size")
print(processed$nmaxchoicesetsize)
print("Dimension of data")
print(dim(processed$data))
print("Dimension of concept matrix")
print(dim(processed$concept))
print("Dimension of code")
print(dim(model$code))
print("Dimension of epsilon")
print(dim(model$epsilon))
print("Dimension of delta")
print(dim(model$delta))
print(model$delta)
print("Dimension of gamma")
print(dim(model$gamma))
print(model$gamma)
print("Dimension of beta")
print(dim(model$beta))
print(model$beta)
print("Dimension of phi")
print(dim(model$phi))
print(model$phi)
print('Number of initial values')
print(length(model$initialvalues))
print("Initial values")
print(model$initialvalues)
if (length(model$initialvalues)==parcount[1]){
print('You have the correct number of initial values.')
}else{
print('ERROR - you have the incorrect number of initial values.')
}
load('Draws.RData')
nrc=dim(model$epsilon)[1]+dim(model$delta)[1]
print("Number of random components")
print(nrc)
print("Number of draws")
print(model$ndraws)
drawsmatrix=matrix(c(1:((model$ndraws*nrc)*0)),model$ndraws,nrc)
print("Dimensions of drawsmatrix")
print(dim(drawsmatrix))
print("Dimensions of draws that are available")
if (model$ndraws==100) {
print(dim(draws$draws1))
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws1[i1,i2]
}}
}
if (model$ndraws==1000) {
print(dim(draws$draws2))
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws2[i1,i2]
}}
}
if (model$ndraws==10000) {
print(dim(draws$draws3))
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws3[i1,i2]
}}
}
print('Draws date stamp')
print(draws$datestamp)
print("Dimension of drawsmatrix")
print(dim(drawsmatrix))
if (runoption == 0){
if (parcount[8]==0){
loglik0=jessyll(model$initialvalues,model,processed$concept,processed$nmaxchoicesetsize,processed$data,processed$ndecisionmakers,drawsmatrix)
} else{
print('correlations not operational yet')
}
resultname=paste('Result0',modelname)
print('Result file name')
print(resultname)
save(file=resultname,list='modelname','model','loglik0')
print("Loglikelihood at initial values")
print(loglik0)
loglikf=loglik0
}
if (runoption == 1){
datestart <- date()
loglik1=jessyllmax(parcount,model$initialvalues,model,processed$concept,processed$nmaxchoicesetsize,processed$data,processed$ndecisionmakers,drawsmatrix)
resultname=paste('Result1',modelname)
print('Result file name')
print(resultname)
 
print("Standard errors")
standarderrors <- sqrt(diag(solve(loglik1$hessian)))
# print(standarderrors)
datefinish <- date()

printpara <- as.matrix(c(1:7)*0)
row.names(printpara) <- c("epsilon_mu","epsilon_sig","delta_mu","delta_sig","gamma","beta","phi")

for (i1 in 1:npp){
  if (model$epsilon[i1,1]==1){printpara[1]=printpara[1]+1}
  if (model$epsilon[i1,2]==1){printpara[2]=printpara[2]+1}
}

for (i1 in 1:nhop){
  if (model$delta[i1,1]==1){printpara[3]=printpara[3]+1}
  if (model$delta[i1,2]==1){printpara[4]=printpara[4]+1}
}

for (i1 in 1:npp){
  for (i2 in 1:nhop){
    if (model$gamma[i1,i2]==1){printpara[5]=printpara[5]+1}
  }
}

for (i1 in 1:nhop){
  for (i2 in 1:nhop){
    if (model$beta[i1,i2]==1){printpara[6]=printpara[6]+1}
  }
}

parastems <- c(rep(("epsilon"),printpara[1]), rep(("epsilon_sig"),printpara[2]), rep(("delta_mu"),printpara[3]), rep(("delta_sig"),printpara[4]), rep(("gamma"),printpara[5]), rep(("beta"),printpara[6]))

subscripts <- matrix(rbind(which(model$epsilon >0, arr.ind = T), which(model$delta >0, arr.ind = T), which(model$gamma >0, arr.ind = T), which(model$beta >0, arr.ind = T)),nrow = sum(printpara), ncol = 2)

parameters <- paste0(parastems,"_","[",subscripts[,1],",",subscripts[,2],"]")

results <<- data.frame(parameters=parameters, estimate=loglik1$estimate, standarderrors=standarderrors)

results$LL <<-  c(loglik1$minimum,rep(".",nrow(results)-1))

modelnamein <- modelname
modelin <- model
save(file=resultname,list='modelnamein','modelin','loglik1','results')
# source('jessyparprint.R')
# jessyparprint(resultname)
print('Results file name')
print(resultname)
print('Loglikelihood at optimised values')
print(loglik1)
loglikf=loglik1
}
loglikf
}