jessyparcount<-function(model){
model
npp=model$npp
nhop=model$nhop
#epsilon=model$epsilon
#delta=model$delta
#gamma=model$gamma
#beta=model$beta
#phi=model$phi
parcount <- c(1:8)*0
# 1 is total
# 2 is number of epsilon mu
# 3 is number of epsilon sig
# 4 is number of delta mu
# 5 is number of delta sig
# 6 is number of gamma
# 7 is number of beta
# 8 is number of phi
for (i1 in 1:npp){
if (model$epsilon[i1,1]==1){parcount[2]=parcount[2]+1}
if (model$epsilon[i1,2]==1){parcount[3]=parcount[3]+1}
}
for (i1 in 1:nhop){
if (model$delta[i1,1]==1){parcount[4]=parcount[4]+1}
if (model$delta[i1,2]==1){parcount[5]=parcount[5]+1}
}
for (i1 in 1:npp){
for (i2 in 1:nhop){
if (model$gamma[i1,i2]==1){parcount[6]=parcount[6]+1}
}
}
for (i1 in 1:nhop){
for (i2 in 1:nhop){
if (model$beta[i1,i2]==1){parcount[7]=parcount[7]+1}
}
}
for (i1 in 1:(npp+nhop-1)){
for (i2 in (i1+1):(npp+nhop)){
if (model$phi[i1,i2]==1){
if (model&phi[i2,i1]==1){
parcount[8]=parcount[8]+1
}}}}
parcount[1]=parcount[2]+parcount[3]+parcount[4]+parcount[5]+parcount[6]+parcount[7]+parcount[8]
parcount
}