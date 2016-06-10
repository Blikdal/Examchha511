#' @export
#' @author Christian Blikdal Hansen
#' @title Buffonsexp
#' @keywords Buffon Pi Estimation needle
#' @return Estimated pi
#' @example Buffonsexp(10000, 1, 1)
#' @param N <- number of needles thrown
#' @param l <- length of needle
#' @param d <- distance between lines on the floor

Buffonsexp <- function(N, l, d){
  Hits <- 0
  for(i in 1:N){
    u1 <- runif(1)
    u2 <- runif(1)
    phit <- l*sin(u1*pi)+d*u2
    if(phit>d){
      Hits <- Hits+1
    }
  }
  2*N/Hits
}
