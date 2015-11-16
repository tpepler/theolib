vec<-function(datamat)
{
  # Stacks the columns of a data matrix in a vector

  n<-nrow(datamat)
  outputvec<-NULL
  for(i in 1:n){
    outputvec<-append(outputvec,datamat[,i])
  }
  return(outputvec)
}
