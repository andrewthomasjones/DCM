#library(devtools)
#devtools::install_git('ssh://git@github.com/andrewthomasjones/DCM.git')
#install.packages(file.choose(), repos=NULL)
#library(tictoc)
library(DCM)
#
# library(lintr)
# lint_package(linters = linters_with_defaults(
#  line_length_linter = line_length_linter(100),
#  object_name_linter= object_name_linter(c("snake_case","camelCase"))))

#filename<-'/Users/uqajon14/Downloads/values_data_waves1234.txt'

#built in examples
processedBW<-setUp(BWpriorities)
processedDCE<-setUp(DCEpriorities)

filename<-'/Users/uqajon14/Downloads/R Code_RP SP/RP_SP.txt'
processedRP_SP<-setUp(filename, header = FALSE)

processedRP_SP_12345 <- select_variables(processedRP_SP, c("V1", "V2", "V3", "V4", "V5"))
m1RPSP<-model_generator(processedRP_SP_12345, "fixed")
m1RPSP_fit1 <- runModel(m1RPSP)
g<-fixed_model_graph(m1RPSP_fit1)


BW_f<-model_generator(processedBW, "fixed")
BW_f_1 <- runModel(BW_f)
g<-fixed_model_graph(BW_f_1)



m3RPSP_no20<-model_generator(processedRP_SP_no20, "one-factor")


m2RPSP_123<-model_generator(processedRP_SP_123, "random")
m3RPSP_123<-model_generator(processedRP_SP_123, "one-factor")

m1RPSP_no20<-model_generator(processedRP_SP_no20, "fixed")
m2RPSP_no20<-model_generator(processedRP_SP_no20, "random")
m3RPSP_no20<-model_generator(processedRP_SP_no20, "one-factor")


m1BW<-model_generator(processedRP_SP, "fixed")
m2BW<-model_generator(processedRP_SP, "random")
m3BW<-model_generator(processedRP_SP, "one-factor")

m1BW<-model_generator(processedBW, "fixed")
m2BW<-model_generator(processedBW, "random")
m3BW<-model_generator(processedBW, "one-factor")

m1DCE<-model_generator(processedDCE, "fixed")
m2DCE<-model_generator(processedDCE, "random")
m3DCE<-model_generator(processedDCE, "one-factor")

#remove extra var and join - why ?
processedBW2 <- remove_variables(processedBW, "Affordability_BW")

choice_model_vif(processedBW)
choice_model_vif(processedBW2)

processedBWDCE <- join_choicedatasets(processedBW2, processedDCE)

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
