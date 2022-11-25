jessydraws <- function(nrc) {

ndraws = 100
draws1 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
print(p)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
print(mt)
mt = mt[order(mt[,3]),] 
print(mt)
draws1[,i1]=mt[,2]
}
print(draws1)


ndraws = 1000
draws2 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
print(p)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
print(mt)
mt = mt[order(mt[,3]),] 
print(mt)
draws2[,i1]=mt[,2]
}


ndraws = 10000
draws3 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
print(p)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
print(mt)
mt = mt[order(mt[,3]),] 
print(mt)
draws3[,i1]=mt[,2]
}
datestamp=date()
draws=list(draws3=draws3,draws2=draws2,draws1=draws1,datestamp=datestamp)
save(file='Draws.RData',list='draws')
date
print('q=1  ndraws=100')
print(dim(draws1))
print(cor(draws1))
print('q=2  ndraws=1000')
print(dim(draws2))
print(cor(draws2))
print('q=3  ndraws=10000')
print(dim(draws3))
print(cor(draws3))


}
