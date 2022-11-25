jessyllmax <-function(parcount,initialvalues,model,conceptcode,nmaxchoicesetsize,datablock,ndecisionmakersblock,drawsmatrix){
if (parcount[8]==0){
datejessyrunstart=date()
source('jessyll.R')
codecheck=100
icodecheck=0
while (codecheck>1){
loglik <- nlm(jessyll,p=initialvalues   ,model,conceptcode,nmaxchoicesetsize,datablock,ndecisionmakersblock,drawsmatrix,hessian = TRUE)
icodecheck=icodecheck+1
print('Printing from jessyllmax     icodecheck')
print(icodecheck)
print('Printing from jessyllmax     loglik$code')
print(loglik$code)
codecheck=loglik$code
initialvalues=loglik$estimate
resultnametemp=paste('Resulttemp',icodecheck,'.RData')
print('Printing from jessyllmax     temp data file name')
print(resultnametemp)
save.image(resultnametemp)
}
print('Printing from jessyllmax')
print('Number of loops')
print(icodecheck)
print("Loglikelihood for optimized parameters")
print(loglik$minimum)
print("Parameter estimates")
print(loglik$estimate)
print("Standard errors")
print(sqrt(diag(solve(loglik$hessian))))
datejessyrunfinish <- date()
loglik
}
else{
print('Correlations not operational yet')
}
}
