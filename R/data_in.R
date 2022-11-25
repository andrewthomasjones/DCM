#' Read data. This documentation is incomplete.
#'
#' @returns A dataframe of the input data.
#' @examples
#' x<-read_data(test.txt)
#' x<-read_data(test.xlsx)
#' @export
read_data <-function(filename){
#checks file type and then reads accordingly
    if(tools::file_ext(filename)=="txt"){
    data<-read.table(filename)
  }else if(tools::file_ext(filename)=="csv"){
    data<-read.csv(filename)
  }else if(tools::file_ext(filename)=="xls"|file_ext(filename)=="xlsx"){
    data<-as.data.frame(readxl::read_excel(filename, sheet = 1))
  }
  return(data)
}


