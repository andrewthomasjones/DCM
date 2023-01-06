#' @export
frequencyDistribution<-function(cs){

  coldd<-cs$data[,1]
  cold<-matrix(coldd,length(coldd),1)

  s1<-dim(cold)
  i3<-1

  fd<-matrix((1:s1[1]*2)*0,s1[1],2)
  fd[1,1]<-cold[1,1]
  fd[1,2]<-1

  for(i1 in 2:s1[1]){
    not_found<-1
    for(i2 in 1:i3){
       if(cold[i1,1]==fd[i2,1]){
        fd[i2,2]<-fd[i2,2]+1
        not_found<-0
      }
    }
    if(not_found==1){
      i3<-i3+1
      fd[i3,1]<-cold[i1,1]
      fd[i3,2]<-1
    }

  }
  fdd<-matrix((1:i3*2)*0,i3,2)
  fdd[1,1]<-fd[1,1]
  fdd[1,2]<-fd[1,2]

  for(i1 in 2:i3){
    fdd[i1,1]<-fd[i1,1]
    fdd[i1,2]<-fd[i1,2]
    i4<-i1

    while(i4>1){
      if (fdd[i4,1]<fdd[i4-1,1]){
        temp1<-fdd[i4,1]
        temp2<-fdd[i4,2]
        fdd[i4,1]<-fdd[i4-1,1]
        fdd[i4,2]<-fdd[i4-1,2]
        fdd[i4-1,1]<-temp1
        fdd[i4-1,2]<-temp2
      }
      i4<-i4-1
    }
  }
  #print(fdd)
  return(fdd)
}

#' @export
frequencyDistribution2<-function(cs){

  coldd<-cs$data[,1]
  cold<-matrix(coldd,length(coldd),1)

  fdd<-frequencyDistributionCpp(cold)

  return(fdd)
}

