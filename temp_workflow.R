#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
#library(tictoc)
library(DCM)

#lint_package(linters = linters_with_defaults(
#  line_length_linter = line_length_linter(100),
#  object_name_linter= object_name_linter(c("snake_case","camelCase"))))

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'
#filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'

processedBW<-setUp(BWpriorities)
processedDCE<-setUp(DCEpriorities)

m1BW<-model_generator(processedBW, "fixed")
m2BW<-model_generator(processedBW, "random")
m3BW<-model_generator(processedBW, "one-factor")

m1DCE<-model_generator(processedDCE, "fixed")
m2DCE<-model_generator(processedDCE, "random")
m3DCE<-model_generator(processedDCE, "one-factor")

processedBWDCE <- join(processedBW, processedDCE)

m1BWDCE<-model_generator(processedBWDCE, "fixed")
m2BWDCE<-model_generator(processedBWDCE, "random")
m3BWDCE<-model_generator(processedBWDCE, "one-factor")
#m4BWDCE<-model_generator(processedBWDCE , "mtmm")

r1BW<-runModel(m1BW)
r2BW<-runModel(m2BW)
r3BW<-runModel(m3BW)

r1DCE<-runModel(m1DCE)
r2DCE<-runModel(m2DCE)
r3DCE<-runModel(m3DCE)

r1BWDCE<-runModel(m1BWDCE)
r2BWDCE<-runModel(m2BWDCE)
r3BWDCE<-runModel(m3BWDCE)
#r4BWDCE<-runModel(m4BWDCE)




#
#
#
#
#
# summariseModelList(list(r1,r2,r3))#, r4))
#
# parPrintOld(r1)
# parPrintOld(r2)
# parPrintOld(r3)
# #parPrintOld(r4)


#runModel(m1BW)
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
