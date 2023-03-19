#' @export
join <- function(data1,data2){

  #from each input
  concept_1<-data1$concept
  concept_2<-data2$concept

  nmax_choiceset_size_1<-data1$nmax_choiceset_size
  nmax_choiceset_size_2<-data2$nmax_choiceset_size

  nconcepts_1<-dim(concept1)[1]
  nconcepts_2<-dim(concept2)[1]

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

  names_m <- c(data1$attribute_names, data1$attribute_names)

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
      data_m[i,j]<-data_1[i,j]
    }
    data_m[i,3]<-1
  }

  for (i in 1:nrowsdata_2){
    data_m[i+nrowsdata_1,1]<-data_2[i,1]
    data_m[i+nrowsdata_1,2]<-data_2[i,2]+nconcepts_1
    data_m[i+nrowsdata_1,3]<-2
    data_m[i+nrowsdata_1,4]<-data_2[i,4]

    for (j in 5:ncolsdata2){
      data_m[i+nrowsdata1,j]<-data_2[i,j]+nconcepts_1
    }
  }

  data_m<-data_m[order(data_m[,1]),]

  fdd_m<-frequencyDistribution2(data_m)
  ndecisionmakers_m<-dim(fdd_m)[1]

  processed<-list(
                  data_matrix = data_m,
                  data_name = paste(data1$data_name,data2$data_name),
                  data = list(data1$data, data2$data), #*******
                  ncovariates = concept_list_m$ncovariates,
                  nmax_choiceset_size = nmax_choiceset_size_m,
                  ndecisionmakers = ndecisionmakers_m,
                  concept = concept_m,
                  lcovariates=lcovariates_m,
                  fdd=fdd_m,
                  attribute_names<-names_m
  )

  return(processed)
}
