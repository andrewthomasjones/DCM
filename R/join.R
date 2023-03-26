#' @export
join <- function(data1,data2){

  #from each input
  concept_1<-data1$concept
  concept_2<-data2$concept

  nmax_choiceset_size_1<-data1$nmax_choiceset_size
  nmax_choiceset_size_2<-data2$nmax_choiceset_size

  nconcepts_1<-dim(concept_1)[1]
  nconcepts_2<-dim(concept_2)[1]

  ncovariates_1<-data1$ncovariates
  ncovariates_2<-data2$ncovariates

  nrowsdata_1<-dim(data1$data_matrix)[1]
  nrowsdata_2<-dim(data2$data_matrix)[1]

  ncolsdata_1<-dim(data1$data_matrix)[2]
  ncolsdata_2<-dim(data2$data_matrix)[2]

  #combined values
  dlength_m <- nrowsdata_1+nrowsdata_2
  ncolsdata_m <- max(ncolsdata_1,ncolsdata_2)
  ncovariates_m <- ncovariates_1+ncovariates_2
  nconcepts_m <- nconcepts_1+nconcepts_2

  names_m <- c(data1$attribute_names, data2$attribute_names)

  nmax_choiceset_size_m<-max(nmax_choiceset_size_1, nmax_choiceset_size_2)

  concept_m<-matrix(0,nconcepts_m,ncovariates_m)
  data_m<-matrix(0,dlength_m,ncolsdata_m)

  #fill in the complicated ones
  for (i in 1:nconcepts_1){
    for (j in 1:ncovariates_1){
      concept_m[i,j]<-concept_1[i,j]
    }}
  for (i in 1:nconcepts_2){
    for (j in 1:ncovariates_2){
      concept_m[i+nconcepts_1,j+ncovariates_1]<-concept_2[i,j]
    }}

  for (i in 1:nrowsdata_1){
    for (j in 1:ncolsdata_1){
      data_m[i,j]<-data1$data_matrix[i,j]
    }
    data_m[i,3]<-1
  }

  for (i in 1:nrowsdata_2){
    data_m[i+nrowsdata_1,1]<-data2$data_matrix[i,1]
    data_m[i+nrowsdata_1,2]<-data2$data_matrix[i,2]+nconcepts_1
    data_m[i+nrowsdata_1,3]<-2
    data_m[i+nrowsdata_1,4]<-data2$data_matrix[i,4]

    for (j in 5:ncolsdata_2){
      data_m[i+nrowsdata_1,j]<-data2$data_matrix[i,j]+nconcepts_1
    }
  }

  data_m<-data_m[order(data_m[,1]),]

  #need to reformat here otherwise have to fiddle other function too much
  data_fdd<-list()
  data_fdd$data <- data_m

  fdd_m<-frequencyDistribution2(data_fdd)
  ndecisionmakers_m<-dim(fdd_m)[1]


  lcovariates_m<-array(0,  ncovariates_m)

  for (i in 1:ncovariates_m) {
    lcovariates_m[i]=paste('Cov',i)
  }

  processed<-list(
                  data_matrix = list(data1$data_matrix,data2$data_matrix),
                  data_name = paste(data1$data_name,data2$data_name),
                  data = data_m,
                  ncovariates = ncovariates_m,
                  npp = ncovariates_m,
                  nmax_choiceset_size = nmax_choiceset_size_m,
                  ndecisionmakers = ndecisionmakers_m,
                  concept = concept_m,
                  lcovariates=lcovariates_m,
                  fdd=fdd_m,
                  attribute_names=c(data1$attribute_names,data2$attribute_names),
                  attribute_names_all = names_m)

  return(processed)
}
