# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(DCM)

test_check("DCM")

# a<-removeVariables(processedRP_SP, c("V4", "V20"), 1)
# a<-removeVariables(processedRP_SP, c(4, 8), 1)
#
# a<-removeVariables(processedRP_SP, c("ChoiceSet", "V20"), 1)
# a<-removeVariables(processedRP_SP, c(3, 8), 1)
# a<-removeVariables(processedRP_SP, c("V4", "V25"), 1)
# a<-removeVariables(processedRP_SP, c(6, 45), 1)
#
#
# a<-selectVariables(processedRP_SP, c("V4", "V20"), 1)
# a<-selectVariables(processedRP_SP, c(4, 8), 1)
#
# a<-selectVariables(processedRP_SP, c("ChoiceSet", "V20"), 1)
# a<-selectVariables(processedRP_SP, c(3, 8), 1)
# a<-selectVariables(processedRP_SP, c("V4", "V25"), 1)
# a<-selectVariables(processedRP_SP, c(6, 45), 1)
