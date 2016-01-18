hankelize<-function(x){
## Diagonal averaging ('hankelization') as described in Golyandina & Zhigljavsky (2013), p.18
#  x: matrix to be hankelized

  L<-nrow(x)
  K<-ncol(x)
  hankel.mat<-matrix(NA,nrow=L,ncol=K)
  for(i in 1:L){
    for(j in 1:K){
      s<-i+j
      temp.total<-0
      temp.count<-0
      for(l in 1:(s-1)){
        k<-s-l
        if(l <= L & k <= K){
          temp.total<-temp.total+x[l,k]
          temp.count<-temp.count+1
        }
      }
      hankel.mat[i,j]<-temp.total/temp.count
    }
  }
  return(hankel.mat)
}
