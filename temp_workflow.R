#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
library(tictoc)

library(DCM)

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'

processed<-setUp(filename, header = F)

m1<-model_generator(processed, "fixed")
m2<-model_generator(processed, "random")
m3<-model_generator(processed, "one-factor")
#m4<-model_generator(processed, "mtmm") #doesnt work divide by two thing

r1<-runModel(m1)
r2<-runModel(m2)
r3<-runModel(m3)
#r4<-runModel(m4)

summariseModelList(list(r1,r2,r3))#, r4))

parPrintOld(r1)
parPrintOld(r2)
parPrintOld(r3)
#parPrintOld(r4)




#
#
# dataname=paste("./DATA/",select.list(list.files(path = "./DATA", pattern = ".RData"), multiple = FALSE,
#                                      title = "Choose your dataset", graphics = TRUE),sep="")
# load(dataname)
#
# EMIs <- as.matrix(select.list(list.files(path = "./EMIs", pattern = "EMI_"), preselect = NULL, multiple = TRUE,
#                               title = "Model specification (hold CTRL to select multiple", graphics = TRUE))
#
# descriptions <- as.matrix(file_path_sans_ext(EMIs))
# descriptions
#
# ndraws <<- as.integer(select.list(c("100","1000","10000"), multiple = FALSE,
#                                   title = "Specify the resample size", graphics = TRUE))
#
# ndraws_save <<- ndraws
# ll_list <- vector()
# ncov_list <- vector()
# nhop_list <- vector()
# AIC_list <- vector()
# BIC_list <- vector()
#
# for (i in 1:nrow(descriptions)){
#   description=descriptions[i]
#
#   source('./R/KOBEemi.R')
#   source('./R/KOBEcheckmodel.R')
#   source('./R/KOBEresample.R')
#
#   KOBEcheckmodel(paste('./WORKSPACE/',description,".RData",sep=""))
# }
#
# model_catalogue <- matrix(cbind(c(descriptions,nhop_list,ncov_list,ll_list,AIC_list,BIC_list)),ncol=6)
# colnames(model_catalogue) <- c("Model","C","K","LL","AIC","BIC")
# rownames(model_catalogue) <- paste0(rep("M",nrow(model_catalogue)),1:nrow(model_catalogue))
# print(model_catalogue)
# write.csv(model_catalogue, paste0("./RESULTS/","RESULTS_CATALOGUE_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv"))
#
