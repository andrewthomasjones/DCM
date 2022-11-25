read_data <-function(filename){
#checks file type and then reads accordingly
    if(file_ext(filename)=="txt"){
    data<-read.table(filename)
  }else if(file_ext(filename)=="csv"){
    data<-read.csv(filename)
  }else if(file_ext(filename)=="xls"|file_ext(filename)=="xlsx"){
    data<-as.data.frame(read_excel(filename, sheet = 1))
  }
  return(data)
}


