EMI <- openxlsx::loadWorkbook(paste("./EMIs/",description,".xlsx",sep=""), isUnzipped = FALSE)
ncovariates=nrow(read.xlsx(EMI,sheet = 1))
npp=ncovariates
nhop=nrow(read.xlsx(EMI, sheet = 2))
code     = matrix(c(1:ncovariates*npp)*0      ,ncovariates,npp)
epsilon  = as.matrix(read.xlsx(EMI, sheet = 1, cols = c(2,3)))
delta    = as.matrix(read.xlsx(EMI, sheet = 2, cols = c(2,3)))
gamma    = as.matrix(read.xlsx(EMI, sheet = 3, cols = c(2:(1+nhop))))
beta     = as.matrix(read.xlsx(EMI, sheet = 4, cols = c(2:(1+nhop))))
phi      = matrix(c(1:(npp+nhop))*0           ,npp+nhop   ,npp+nhop)
for (i1 in 1:(npp+nhop)) phi[i1,i1] = 1
if (ncovariates==npp) { for (i1 in 1:npp) code[i1,i1] = 1 }

initial_e <-  as.matrix(read.xlsx(EMI, sheet = 5, cols = c(2,3)))
initial_e <-  na.omit(as.vector(initial_e))
initial_d <-  as.matrix(read.xlsx(EMI, sheet = 6, cols = c(2,3)))
initial_d <-  na.omit(as.vector(initial_d))
initial_g <-  as.matrix(read.xlsx(EMI, sheet = 7, cols = c(2:(1+nhop))))
initial_g <-  na.omit(as.vector(initial_g))
initial_b <-  as.matrix(read.xlsx(EMI, sheet = 8, cols = c(2:(1+nhop))))
initial_b <-  na.omit(as.vector(initial_b))
initialvalues <- c(initial_e,initial_d,initial_g,initial_b)

model=list(description=description,dataname=dataname,ndraws=ndraws,ncovariates=ncovariates,npp=npp,nhop=nhop,code=code,epsilon=epsilon,delta=delta,gamma=gamma,beta=beta,phi=phi,initialvalues=initialvalues)
save(file=paste("./WORKSPACE/",description,".RData",sep=""),model)