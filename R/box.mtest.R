box.mtest<-function(covmats, nvec)
{
  # Chi-square approximation to Box's M test for equality of k covariance matrices, as described in Rencher (2002, p255). Returns the p value for the test.

  # covmats: array of k covariance matrices to be tested for equality
  # nvec: vector of sample sizes of the k groups

  k<-dim(covmats)[3]
  p<-dim(covmats)[2]
  ntot<-sum(nvec)

  Sp<-matrix(0,nrow=p,ncol=p)
  for(i in 1:k){
    Sp<-Sp+(nvec[i]-1)*covmats[,,i]
  }
  Sp<-Sp/(ntot-k)

  temp<-0
  for(i in 1:k){
    temp<-temp + 1/(nvec[i]-1)
  }

  c1<-(temp-(1/(ntot-k)))*(2*(p^2)+3*p-1)/(6*(p+1)*(k-1))

  temp2<-0
  for(i in 1:k){
    temp2<-temp2 + (nvec[i]-1)*log(det(covmats[,,i]))
  }

  lnM<-0.5*(temp2)-0.5*(ntot-k)*log(det(Sp))

  chi2<--2*(1-c1)*lnM

  v<-0.5*(k-1)*p*(p+1)

  pval<-pchisq(q=chi2, df=v, lower.tail = F)

  return(pval)
}
