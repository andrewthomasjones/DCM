# KOBEdraws sets up the resample file... maybe it should be renamed to KOBEresample!

#suppressMessages(suppressWarnings(require("svDialogs")))

if (!file.exists(paste('./RESAMPLE/resample_',description,'.RData',sep = ""))){

KOBEdraws <- function(nrc) {

ndraws = 100
draws1 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
mt = mt[order(mt[,3]),] 
draws1[,i1]=mt[,2]
}


ndraws = 1000
draws2 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
mt = mt[order(mt[,3]),] 
draws2[,i1]=mt[,2]
}


ndraws = 10000
draws3 = matrix((1:(ndraws*nrc))*0,ndraws,nrc)
p=(1:(ndraws))/(ndraws+1)
for (i1 in 1:nrc){
mt = matrix(1:(3*ndraws),ndraws,3) 
mt[,1]=t(p)
mt[,2]=t(qnorm(mt[,1]))
mt[,3]=t(runif(ndraws))
mt = mt[order(mt[,3]),] 
draws3[,i1]=mt[,2]
}

datestamp=date()
draws=list(draws3=draws3,draws2=draws2,draws1=draws1,datestamp=datestamp)
save(file=paste('./RESAMPLE/resample_',description,'.RData',sep = ""),list='draws')
cat("\n resample.RData has been saved to ",getwd(),"/RESAMPLE",sep="")
}
ndraws=npp+nhop
KOBEdraws(ndraws)
} else {(print("Using the resample already generated for this model earlier..."))}
ndraws=npp+nhop*2
# This is where I set the ndraws to npp plus nhop.  Once specified, the KOBEdraws() function creates 3 draws matrices
# that have the same dimensions, but different sizes in terms of rows (i.e. resample SIZE, where rows = [re]samples)
# the first   has 100 rows and npp+nhop columns,
# the second  has 1000 rows and npp+nhop columns,
# the third   has 10000 rows and npp+nhop columns,
# The dimensionality needs to be at least as wide as the model, hence npp+nhop
# When the model runs, you are asked to specify which of these three matrices to use (resample size 100, 1000 or 10000)
# To be clear: all three matrices are created, regardless of how wide you specify the dimensionality to be
# To be clear2: we only get to choose the dimensionality of the matrices (width)
# In estimation, which of these matrices we choose to use seems to be inconsequential other than for larger resamples
# being slow. I don't know how in DISCOS achieved marked improvement by using larger resamples (even for the fixed MNL
# Something I will test later is if larger resamples provide more stability for more complex latent structures...
# I have done robust testing of this last point as such models take a very long time of running before I determine if
# the model is failing or will converge...

