jessyparprint<-function(result1){
load(result1)
source('jessyparcount.R')
parcountin=jessyparcount(modelin)
print(parcountin)
resprint=c(1:parcountin[1]+8)*0
resprint[1]=paste('Printout of results for ',result1,'\\\\')
resprint[2]=paste('log likelihood',round(loglik1$minimu,4),'\\\\')
resprint[3]=paste('Number of parameters',parcountin[1],'\\\\')
resprint[4]='$\\\\'
resprint[5]='\\begin{array}{lrr}'
resprint[6]=paste('Model Parameter & Estimate & Standard Error\\\\')
resprinti=6
for (i1 in 1:modelin$npp){
if (modelin$epsilon[i1,1]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\epsilon_{\\mu,',i1,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}
for (i1 in 1:modelin$nhop){
if (modelin$delta[i1,1]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\delta_{\\mu,',i1,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}
for (i1 in 1:modelin$npp){
if (modelin$epsilon[i1,2]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\epsilon_{\\sigma,',i1,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}
for (i1 in 1:modelin$nhop){
if (modelin$delta[i1,2]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\delta_{\\sigma,',i1,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}
for (i1 in 1:modelin$npp){
for (i2 in 1:modelin$nhop){
if (modelin$gamma[i1,i2]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\gamma_{',i1,',',i2,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}}
for (i1 in 1:modelin$nhop){
for (i2 in 1:modelin$nhop){
if (modelin$beta[i1,i2]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\beta_{',i1,',',i2,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}}
for (i1 in 1:(modelin$npp+modelin$nhop-1)){
for (i2 in (i1+1):(modelin$npp+modelin$nhop)){
if (modelin$phi[i1,i2]==1){
if (modelin&phi[i2,i1]==1){
resprinti=resprinti+1
resprint[resprinti]=paste('\\phi_{',i1,',',i2,'}&',round(loglik1$estimate[i1],4),'&',round(results$standarderrors[i1],4),'\\\\')
}}}}
resprinti=resprinti+1
resprint[resprinti]='\\end{array}'
resprinti=resprinti+1
resprint[resprinti]='$\\\\'

textname=paste('outtext',result1,'.txt')
print(textname)
write(resprint,file=textname)
print(resprint)
}
