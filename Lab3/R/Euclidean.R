#' Find the greatest common divisor.
#' 
#' @param a an integer.
#' @param b an integer.
#' @return The GCD of \code{a} and \code{b}.
#' @examples
#' euclidean(123612,13892347912)
#' euclidean(100,1000)
#' \code{\link{https://en.wikipedia.org/wiki/Euclidean_algorithm}}

euclidean<-function(a, b){
stopifnot(a%%1==0, b%%1==0)
  while (b!=0)
  {t <- b; 
  b <- a %% t; 
  a <- t; 
  }
  return(abs(a))
}
