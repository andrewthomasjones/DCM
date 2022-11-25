print("Calculating average choice probabilities (in acp)")
source('jessyloglikelihoodlvdce.R')
workingvaluesacp <- parameterestimates
datablock       <- data
ndecisionmakersblock <- ndecisionmakers
camansacp      <- jessyloglikelihoodlvdce(workingvaluesacp)
print("Original LL for whole dataset")
print(-camansacp)  
nlinesdataone   <- 0
nlinesdatatwo   <- 0
i1              <- 1
i2              <- 0
lacpag          <- (1:ndecisionmakers)*0
lacpagone       <- (1:ndecisionmakers)*0
lacpagtwo       <- (1:ndecisionmakers)*0
lacpbg          <- (1:ndecisionmakers)*0
lacpbgone       <- (1:ndecisionmakers)*0
lacpbgtwo       <- (1:ndecisionmakers)*0
lacpxgone       <- (1:ndecisionmakers)*0
lacpxgtwo       <- (1:ndecisionmakers)*0
lacpaa          <- (1:ndecisionmakers)*0
lacpaaone       <- (1:ndecisionmakers)*0
lacpaatwo       <- (1:ndecisionmakers)*0
lacpag          <- (1:ndecisionmakers)*0
lacpagone       <- (1:ndecisionmakers)*0
lacpagtwo       <- (1:ndecisionmakers)*0
lacpba          <- (1:ndecisionmakers)*0
lacpbaone       <- (1:ndecisionmakers)*0
lacpbatwo       <- (1:ndecisionmakers)*0
lacpxaone       <- (1:ndecisionmakers)*0
lacpxatwo       <- (1:ndecisionmakers)*0

datablockdm     <- data[1,]
iddm            <- data[1,1]
ndecisionmakersblock <- 1

while (i1<nlinesdata) {
i1              <- i1+1
if (data[i1,1]==iddm) {
datablockdm     <- rbind(datablockdm,data[i1,])
}
if (data[i1,1]>iddm) {
source("jessyacpworking.R")
iddm            <- data[i1,1]
datablockdm     <- data[i1,]
}
}
source("jessyacpworking.R")


print("________________________________________")
print("Number of choices DCE #1 and #2")
print(nlinesdata)
print("Number of choices DCE #1")
print(nlinesdataone)
print("Number of choices DCE #2")
print(nlinesdatatwo)
print("Avergare choice set size DCE #1 and #2")
print("Avergare choice set size DCE #1")
print("Avergare choice set size DCE #2")

acpall          <- matrix((1:(5*6))*0,5,6)
acpall[1,1]     <- exp(sum(lacpagone)/nlinesdataone)
acpall[1,2]     <- exp(sum(lacpagtwo)/nlinesdatatwo)
acpall[1,3]     <- exp(sum(lacpag)/nlinesdata)
acpall[2,1]     <- exp(sum(lacpbgone)/nlinesdataone)
acpall[2,2]     <- exp(sum(lacpbgtwo)/nlinesdatatwo)
acpall[2,3]     <- exp(sum(lacpbg)/nlinesdata)
acpall[4,1]     <- exp(sum(lacpxgone)/nlinesdataone)
acpall[4,2]     <- exp(sum(lacpxgtwo)/nlinesdatatwo)

acpall[1,4]     <- sum(lacpaaone)/nlinesdataone
acpall[1,5]     <- sum(lacpaatwo)/nlinesdatatwo
acpall[1,6]     <- sum(lacpaa)/nlinesdata
acpall[2,4]     <- sum(lacpbaone)/ndecisionmakers
acpall[2,5]     <- sum(lacpbatwo)/ndecisionmakers
acpall[2,6]     <- sum(lacpba)/ndecisionmakers
acpall[4,4]     <- sum(lacpxaone)/nlinesdataone
acpall[4,5]     <- sum(lacpxatwo)/nlinesdatatwo
print(acpall)

lacpall         <- matrix((1:(5*7))*0,5,7)
lacpall[1,3]    <- sum(lacpag)
lacpall[1,1]    <- sum(lacpagone)
lacpall[1,2]    <- sum(lacpagtwo)
lacpall[2,3]    <- sum(lacpbg)
lacpall[2,1]    <- sum(lacpbgone)
lacpall[2,2]    <- sum(lacpbgtwo)
lacpall[4,1]    <- sum(lacpxgone)
lacpall[4,2]    <- sum(lacpxgtwo)
lacpall[1,6]    <- sum(lacpaa)
lacpall[1,4]    <- sum(lacpaaone)
lacpall[1,5]    <- sum(lacpaatwo)
lacpall[2,6]    <- sum(lacpba)
lacpall[2,4]    <- sum(lacpbaone)
lacpall[2,5]    <- sum(lacpbatwo)
lacpall[4,4]    <- sum(lacpxaone)
lacpall[4,5]    <- sum(lacpxatwo)
lacpall[2,7]    <- -camansacp

print(lacpall)

