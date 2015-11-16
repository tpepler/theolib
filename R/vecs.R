vecs<-function(datamat)
{
  # Stacks the columns of the lower diagonal of a symmetric matrix in a vector

  p<-ncol(datamat)
  outputvec<-NULL
  for(j in 1:p){
    outputvec<-append(outputvec,datamat[j:p,j])
  }
  return(outputvec)
}
