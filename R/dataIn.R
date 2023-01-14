#' Read data. This documentation is incomplete.
#'
#' @returns A dataframe of the input data.
#' @examples
#' x<-read_data(test.txt)
#' x<-read_data(test.xlsx)
#' @export
readData <-function(filename, header=TRUE){
  #checks file type and then reads accordingly
  if(tools::file_ext(filename)=="txt"){
    data<-read.table(filename, header = header)
  }else if(tools::file_ext(filename)=="csv"){
    data<-read.csv(filename, header = header)
  }else if(tools::file_ext(filename)=="xls"|file_ext(filename)=="xlsx"){
    data<-as.data.frame(readxl::read_excel(filename, sheet = 1, col_names = header))
  }
  return(data)
}


