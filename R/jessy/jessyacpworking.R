dimdatablockdm <- dim(datablockdm)

datablock <- datablockdm
print("Data block decision maker (at acpworking)")
print(datablock)
camansacpb      <- jessyloglikelihoodlvdce(workingvaluesacp)
i2              <- i2+1
lacpbg[i2]      <- -camansacpb
lacpba[i2]      <- exp(-camansacpb/dimdatablockdm[1])


i3              <- 0
i4              <- 0
i5              <- 0

while (i3<dimdatablockdm[1]){
i3              <- i3+1
datablock       <- matrix(datablockdm[i3,],1,dimdatablockdm[2])
print(datablock)
camansacpa      <- jessyloglikelihoodlvdce(workingvaluesacp)
lacpag[i2]      <- lacpag[i2]-camansacpa
lacpaa[i2]      <- lacpaa[i2]+exp(-camansacpa)

if (datablockdm[i3,3]<=1){
lacpagone[i2]   <- lacpagone[i2]-camansacpa
lacpaaone[i2]   <- lacpaaone[i2]+exp(-camansacpa)
if (i4>0) datablockone <- rbind(datablockone,datablockdm[i3,])
if (i4==0) datablockone <- matrix(datablockdm[i3,],1,dimdatablockdm[2])
i4              <- 1
}

if (datablockdm[i3,3]>=2){
lacpagtwo[i2]   <- lacpagtwo[i2]-camansacpa
lacpaatwo[i2]   <- lacpaatwo[i2]+exp(-camansacpa)
if (i5>0) datablocktwo <- rbind(datablocktwo,datablockdm[i3,])
if (i5==0) datablocktwo <- matrix(datablockdm[i3,],1,dimdatablockdm[2])
i5 <- 1
}
}

dimdatablockone <- c(0,0)
if (i4>0){
datablock       <- datablockone
print(datablock)
camansacpbone   <- jessyloglikelihoodlvdce(workingvaluesacp)
lacpbgone[i2]   <- -camansacpbone
dimdatablockone <- dim(datablockone)
nlinesdataone   <- nlinesdataone+dimdatablockone[1]
lacpbaone[i2]   <- exp(-camansacpbone/dimdatablockone[1]) 
}

dimdatablocktwo <- c(0,0)
if (i5>0){
datablock       <- datablocktwo
print(datablock)
camansacpbtwo   <- jessyloglikelihoodlvdce(workingvaluesacp)
lacpbgtwo[i2]   <- -camansacpbtwo
dimdatablocktwo <- dim(datablocktwo)
nlinesdatatwo   <- nlinesdatatwo+dimdatablocktwo[1]
lacpbatwo[i2]   <- exp(-camansacpbtwo/dimdatablocktwo[1]) 
}

for (i3 in 1:dimdatablockone[1]){
datablock       <- rbind(datablocktwo,datablockone[i3,])
print(datablock)
camansacpxone   <- jessyloglikelihoodlvdce(workingvaluesacp)
lacpxgone[i2]   <- lacpxgone[i2]-camansacpxone-lacpbgtwo[i2]
lacpxaone[i2]   <- lacpxaone[i2]+exp(-camansacpxone-lacpbgtwo[i2])}

for (i3 in 1:dimdatablocktwo[1]){
datablock       <- rbind(datablockone,datablocktwo[i3,])
print(datablock)
camansacpxtwo   <- jessyloglikelihoodlvdce(workingvaluesacp)
lacpxgtwo[i2]   <- lacpxgtwo[i2]-camansacpxtwo-lacpbgone[i2]
lacpxatwo[i2]   <- lacpxatwo[i2]+exp(-camansacpxtwo-lacpbgone[i2])
}
