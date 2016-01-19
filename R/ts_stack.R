ts.stack <- function(x, window=floor(length(x)/2)){
  ts.mat<-NULL
  for(i in 1:(length(x)-window+1)){
    ts.mat<-cbind(ts.mat,x[i:(i+window-1)])
  }
  return(ts.mat)
}
