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

#remove extra var and join
processedBW2 <- remove_variable(processedBW, "Accessibility_BW")
processedBWDCE <- join2(processedBW2, processedDCE)

m1BWDCE<-model_generator(processedBWDCE, "fixed")
m2BWDCE<-model_generator(processedBWDCE, "random")
m3BWDCE<-model_generator(processedBWDCE, "one-factor")

#run models
r1BW<-runModel(m1BW)
r2BW<-runModel(m2BW)
r3BW<-runModel(m3BW)

r1DCE<-runModel(m1DCE)
r2DCE<-runModel(m2DCE)
r3DCE<-runModel(m3DCE)

r1BWDCE<-runModel(m1BWDCE)
r2BWDCE<-runModel(m2BWDCE)
r3BWDCE<-runModel(m3BWDCE)

summariseModelList(list(r1BW,r2BW,r3BW))
summariseModelList(list(r1DCE,r2DCE,r3DCE))
summariseModelList(list(r1BWDCE,r2BWDCE,r3BWDCE))

cat(parPrint(r1BW))
cat(parPrint(r2BW))
cat(parPrint(r3BW))

cat(parPrint(r1DCE))
cat(parPrint(r2DCE))
cat(parPrint(r3DCE))

cat(parPrint(r1BWDCE))
cat(parPrint(r2BWDCE))
cat(parPrint(r3BWDCE))
