KOBEcheckmodel <- function(modelname,runoption){
source('./R/KOBEll.R')
source('./R/KOBEllmax.R')
source('./R/KOBEparcount.R')
load(modelname)
parcount<-KOBEparcount(model)
load(model$dataname)
if (length(model$initialvalues)==parcount[1]){
cat('Model checked and you have the correct number of initial values, yippee!')
}else{
print('ERROR - you have the incorrect number of initial values. Sad.')
}
load(paste('./RESAMPLE/resample_',description,'.RData',sep = ""))
cat('\n KOBE is now estimating',description,'using',ndraws,'Halton draws','\n Please wait a moment (or a while)...')
nrc<<-dim(model$epsilon)[1]+dim(model$delta)[1]
drawsmatrix=matrix(c(1:((model$ndraws*nrc)*0)),model$ndraws,nrc)
if (model$ndraws==100) {
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws1[i1,i2]
}}
}
if (model$ndraws==1000) {
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws2[i1,i2]
}}
}
if (model$ndraws==10000) {
for (i1 in 1:model$ndraws){
for (i2 in 1:nrc){
drawsmatrix[i1,i2]  =draws$draws3[i1,i2]
}}
}
datestart <- date()
loglik1=KOBEllmax(parcount,model$initialvalues,model,processed$concept,processed$nmaxchoicesetsize,processed$data,processed$ndecisionmakers,drawsmatrix)
standarderrors <- sqrt(diag(solve(loglik1$hessian)))
datefinish <- date()

# That completes the modelling, now to print the parameters.

toms_parcount <- as.matrix(c(1:6)*0)
row.names(toms_parcount) <- c("epsilon_mu","epsilon_sig","delta_mu","delta_sig","gamma","beta")

#EPSILON MU AND SIGMA [1 and 2]
for (i1 in 1:npp){
  if (model$epsilon[i1,1]==1){toms_parcount[1]=toms_parcount[1]+1}
  if (model$epsilon[i1,2]==1){toms_parcount[2]=toms_parcount[2]+1}
}

#DELTA MU AND SIGMA [3 and 4]
for (i1 in 1:nhop){
  if (model$delta[i1,1]==1){toms_parcount[3]=toms_parcount[3]+1}
  if (model$delta[i1,2]==1){toms_parcount[4]=toms_parcount[4]+1}
}

#GAMMA [5]
for (i1 in 1:npp){
  for (i2 in 1:nhop){
    if (model$gamma[i1,i2]==1){toms_parcount[5]=toms_parcount[5]+1}
  }
}

#BETA [6]
for (i1 in 1:nhop){
  for (i2 in 1:nhop){
    if (model$beta[i1,i2]==1){toms_parcount[6]=toms_parcount[6]+1}
  }
}

toms_parastems <- c(rep(("epsilon"),toms_parcount[1]),
                    rep(("epsilon_sig"),toms_parcount[2]),
                    rep(("delta_mu"),toms_parcount[3]),
                    rep(("delta_sig"),toms_parcount[4]),
                    rep(("gamma"),toms_parcount[5]),
                    rep(("beta"),toms_parcount[6]))

toms_subscripts <- matrix(rbind(
                    which(model$epsilon >0, arr.ind = T),
                    which(model$delta >0, arr.ind = T),
                    which(model$gamma >0, arr.ind = T),
                    which(model$beta >0, arr.ind = T)),
                    nrow = sum(toms_parcount), ncol = 2)

parameters <- paste0(toms_parastems,"_","[",toms_subscripts[,1],",",toms_subscripts[,2],"]")
                            
results <- list(parameters=parameters,
                 estimate=loglik1$estimate,
                 standarderrors=standarderrors,
                 t=loglik1$estimate/standarderrors,
                 CI.lower=loglik1$estimate-(2*standarderrors),
                 CI.upper=loglik1$estimate+(2*standarderrors),
                 LL=c(loglik1$minimum,rep("", times = length(parameters)-1)),
                 nrc=c(ndraws,rep("",times = length(parameters)-1)),
                 resample_size=c(ndraws_save,rep("",times = length(parameters)-1)),
                 datestart=c(datestart,rep("", times = length(parameters)-1)),
                 datefinish=c(datefinish,rep("", times = length(parameters)-1)))

ll_list <<- c(ll_list, results$LL[1]) 
ncov_list <<- c(ncov_list, ncovariates)
nhop_list <<- c(nhop_list, nhop)
AIC_list <<- c(AIC_list,2*ncovariates-2*as.numeric(results$LL[1]))
BIC_list <<- c(BIC_list,log(processed$dlength)*ncovariates)-2*as.numeric(results$LL[1])

write.csv(results, paste0("./RESULTS/","RESULTS_", substr(description,5,99),".csv"))

cat("Results saved as RESULTS_",substr(description,5,99),".csv in ",getwd(),"/RESULTS/",fill=TRUE,sep="")

}