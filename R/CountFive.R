#' @title Use two inputs to test significance level using R.
#' @description A two sample test of equal variance.
#' @param  x the first sample(vector)
#' @param  y the second sample(vector)
#' @return number of extreme points \code{n}
#' @examples
#' \dontrun{
#' number <- CountFive(x,y)
#' }
#' @export
CountFive<-function(x,y){
  X<-x-mean(x)
  Y<-y-mean(y)
  outx<-sum(X>max(x))+sum(X<min(Y))
  outy<-sum(Y>max(X))+sum(Y<min(X))
  return(base::as.integer(max(c(outx,outy))>5))
}




