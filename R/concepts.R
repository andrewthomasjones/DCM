#' @export
createConcepts <- function(data_matrix, nmax_choiceset_size = 31) {
  dim_data_matrix <- dim(data_matrix)
  nlines_data_matrix <- dim_data_matrix[1]
  ncovariates <- dim_data_matrix[2] - 3

  iddm <- data_matrix[1, 1]
  idcs <- data_matrix[1, 2]

  data_big <-
    matrix((1:nlines_data_matrix * (nmax_choiceset_size + 4)) * 0,
           nlines_data_matrix,
           nmax_choiceset_size + 4)
  data_big[1, 1] = iddm

  if (data_matrix[1, 3] == 1){
    data_big[1, 2] <- 1
  }
  data_big[1, 4] <- 1

  concept_big <-
    matrix((1:nlines_data_matrix * ncovariates) * 0,
           nlines_data_matrix,
           ncovariates)
  for (i3 in 1:ncovariates){
    concept_big[1, i3] <- data_matrix[1, i3 + 3]
  }
  data_big[1, 5] <- 1

  i1 <- 1
  i2 <- 1
  i5 <- 1
  i6 <- 1
  i4<-0

  # print("i1 is the count of lines moving through the data matrix")
  # print("i2 is the build of the number of rows in data_big")
  # print("i3 is the count of covariates moving through the row of the data matrix")
  # print("i4 is the count of lines moving through the concept matrix")
  # print("i5 is the build of the number of concepts in a row in data_big")
  # print("i6 is build of the number of concepts")


  while (i1 < nlines_data_matrix) {
    # if(i1<10){
    #   print(paste0(c("i1 " , i1 , " i2 " , i2 ,  " i4 " , i4 , " i5 " , i5 , " i6 " , i6)))
    # }

    i1 <- i1 + 1
    match_number <- 0
    i4 <- 0

    while (i4 < i6) {
      i4 <- i4 + 1
      match_count <- 0

      for (i3 in 1:ncovariates) {
        if (concept_big[i4, i3] == data_matrix[i1, i3 + 3])
          match_count <- match_count + 1
      }

      if (match_count == ncovariates) {
        match_number <- i4
        i4 <- i6
      }
    }
    # if(i1<10){
    #   print(paste0(c( "i4 " ,i4 , " i6 " ,i6)))
    # }

    if (match_number == 0) {
      i6 <- i6 + 1
      match_number <- i6
      for (i3 in 1:ncovariates)
        concept_big[i6, i3] <- data_matrix[i1, i3 + 3]
    }


    if (data_matrix[i1, 1] == iddm) {
      if (data_matrix[i1, 2] == idcs) {
        i5 <- i5 + 1
        data_big[i2, i5 + 4] <- match_number
        if (data_matrix[i1, 3] == 1)
          data_big[i2, 2] <- match_number
      }
    }

    if (data_matrix[i1, 1] == iddm) {
      if (data_matrix[i1, 2] > idcs) {
        #print(data_big[i2,])
        idcs <- data_matrix[i1, 2]
        i5 <- 1
        i2 <- i2 + 1
        data_big[i2, 1] <- iddm
        data_big[i2, 4] <- 1
        data_big[i2, i5 + 4] <- match_number
        if (data_matrix[i1, 3] == 1)
          data_big[i2, 2] <- match_number
      }
    }

    if (data_matrix[i1, 1] > iddm) {
      iddm <- data_matrix[i1, 1]
      idcs <- data_matrix[i1, 2]
      i5 <- 1
      i2 <- i2 + 1
      data_big[i2, 1] <- iddm
      data_big[i2, 4] <- 1
      data_big[i2, i5 + 4] <- match_number
      if (data_matrix[i1, 3] == 1)
        data_big[i2, 2] <- match_number

    }


  }

  concept <- matrix((1:i6 * ncovariates) * 0, i6, ncovariates)
  for (i8 in 1:i6) {
    for (i9 in 1:ncovariates) {
      concept[i8, i9] = concept_big[i8, i9]
    }
  }
  nconcepts <- i6

  data <-
    matrix((1:i2 * (nmax_choiceset_size + 4)) * 0, i2, nmax_choiceset_size +
             4)
  for (i8 in 1:i2) {
    for (i9 in 1:(nmax_choiceset_size + 4)) {
      data[i8, i9] = data_big[i8, i9]
    }
  }
  nlines_data <- i2

  return(
    list(
      data = data,
      nconcepts = nconcepts,
      nlines_data = nlines_data,
      ncovariates = ncovariates,
      data_big = data_big,
      concept = concept
    )
  )
}


#' @export
createConcepts2 <- function(data_matrix, nmax_choiceset_size = 31) {
  concepts <- createConceptsCpp(as.matrix(data_matrix), nmax_choiceset_size)
  return(concepts)
}
