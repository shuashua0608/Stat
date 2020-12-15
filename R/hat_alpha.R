#' @title The significance level for CountFive test.
#' @description Estimate the significance level of the CountFive test .
#' @param x the first sample(vector).
#' @param y the second sample(vector).
#' @return  empirical Type I error rate \code{n}
#' @export
hat_alphat<-function(x,y,m=1e3){
  return( mean(replicate(m, expr={
    x = x - mean(x) #centered by sample mean
    y = y - mean(y)
    CountFive(x, y)
  })))
}