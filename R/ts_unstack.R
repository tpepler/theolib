ts.unstack <- function(x){
  if(!is.matrix(x)){stop('ERROR: \'x\' is not a matrix!')}
  L<-nrow(x)
  K<-ncol(x)
  ts.vec<-rep(NA,times=K+L-1)
  for(i in 1:(K-1)){
    if(sum(x[2:L,i]!=x[1:(L-1),i+1])>0){stop('ERROR: \'x\' is not a Hankel matrix!')}
    else{
      ts.vec[i]<-x[1,i]
      ts.vec[(i+1):(i+L)]<-x[,i+1]
    }
  }
  return(ts.vec)
}
