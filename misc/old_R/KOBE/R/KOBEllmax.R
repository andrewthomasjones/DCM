## KOBEllmax is maximising the likelihood function using nlm()

KOBEllmax <-function(parcount,initialvalues,model,conceptcode,nmaxchoicesetsize,datablock,ndecisionmakersblock,drawsmatrix){
if (parcount[8]==0){
dateKOBErunstart=date()
source('./R/KOBEll.R')
codecheck=100
icodecheck=0
while (codecheck>1){
loglik <- suppressWarnings(nlm(KOBEll,p=initialvalues,model,conceptcode,nmaxchoicesetsize,datablock,ndecisionmakersblock,drawsmatrix,hessian = TRUE,print.level=2, iterlim = 10000))
icodecheck=icodecheck+1
codecheck=loglik$code
initialvalues=loglik$estimate

}
cat('\n ################################################### \n')
cat('\n #############KOBE Modelling Complete!############## \n')
cat('\n ################################################### \n')

dateKOBErunfinish <- date()
loglik
}
else{
print('Correlations not operational yet')
}
}
