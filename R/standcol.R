standcol<-function (x,centre=FALSE,stand=TRUE)
{
  # Standardize columns to have zero mean

  if(centre){
    for(i in 1:ncol(x)){
      x[,i]<-x[,i]-mean(x[,i])
    }
  }

  if(stand){
    for(i in 1:ncol(x)){
      x[,i]<-x[,i]/sd(x[,i])
    }
  }

  return(x)
}
