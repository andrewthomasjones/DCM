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

#' Read data. This documentation is incomplete.
#'
#' @returns A list of the processed data.
#' @examples
#' x<-setUp(test.txt, header=TRUE)
#' x<-setUp(test.xlsx)
#' @export
setUp<-function(filename, header=TRUE){

  #read in data
  data_matrix<-readData(filename, header = header)

  #from Kobe code
  nmax_choiceset_size<-as.numeric(max(unlist(rle(data_matrix[,2])[1])))
    #concept list
  concept_list<-createConcepts2(data_matrix, nmax_choiceset_size)

  #fdd
  fdd<-frequencyDistribution2(concept_list)

  #some more intermediate processing
  ndecisionmakers<-dim(fdd)[1]

  lcovariates<-array(0, concept_list$ncovariates)

  for (i in 1:concept_list$ncovariates) {
    lcovariates[i]=paste('Cov',i)
  }

  #all the initial stuff packaged up
  processed<-list(data_matrix=data_matrix,
                  data_name=filename,
                  data=concept_list$data,
                  ncovariates=concept_list$ncovariates,
                  nmax_choiceset_size=nmax_choiceset_size,
                  ndecisionmakers=ndecisionmakers,
                  concept=concept_list$concept,
                  lcovariates=lcovariates,
                  fdd=fdd)

  return(processed)

}
