library("tools","readxl","openxlsx")
require("tools","readxl","openxlsx")

dataname=paste("./DATA/",select.list(list.files(path = "./DATA", pattern = ".RData"), multiple = FALSE,
                                     title = "Choose your dataset", graphics = TRUE),sep="")
load(dataname)

EMIs <- as.matrix(select.list(list.files(path = "./EMIs", pattern = "EMI_"), preselect = NULL, multiple = TRUE,
                              title = "Model specification (hold CTRL to select multiple", graphics = TRUE))

descriptions <- as.matrix(file_path_sans_ext(EMIs))
descriptions

ndraws <<- as.integer(select.list(c("100","1000","10000"), multiple = FALSE,
                                 title = "Specify the resample size", graphics = TRUE))

ndraws_save <<- ndraws
ll_list <- vector()
ncov_list <- vector()
nhop_list <- vector()
AIC_list <- vector()
BIC_list <- vector()

for (i in 1:nrow(descriptions)){
description=descriptions[i]

source('./R/KOBEemi.R')
source('./R/KOBEcheckmodel.R')
source('./R/KOBEresample.R')

KOBEcheckmodel(paste('./WORKSPACE/',description,".RData",sep=""))
}

model_catalogue <- matrix(cbind(c(descriptions,nhop_list,ncov_list,ll_list,AIC_list,BIC_list)),ncol=6)
colnames(model_catalogue) <- c("Model","C","K","LL","AIC","BIC")
rownames(model_catalogue) <- paste0(rep("M",nrow(model_catalogue)),1:nrow(model_catalogue))
print(model_catalogue)
write.csv(model_catalogue, paste0("./RESULTS/","RESULTS_CATALOGUE_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv"))

      