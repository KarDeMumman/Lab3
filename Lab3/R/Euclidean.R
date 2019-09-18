
euclidean<-function(a, b){
stopifnot(a%%1==0, b%%1==0)
  while (b!=0)
  {t <- b; 
  b <- a %% t; 
  a <- t; 
  }
  return(abs(a))
}