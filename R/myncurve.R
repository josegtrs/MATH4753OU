#' My Normal Curve
#'
#' @param mu = mean of functio
#' @param sigma = standard deviation of function
#' @param a = input of probability and area to be calculated
#'
#' @return a curve
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
#' @export
myncurve = function(mu=10,sigma=5, b=6){
  b = b
  a = mu-(3*sigma)
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(a,b, length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(a,xcurve,b),c(0,ycurve,0),col="Pink")
  # p(y < a) + p(y >= a) = 1
  prob= 1 - pnorm(b,mu,sigma )
  area=round(prob,4)
  text((a+b)/2, .5*.1 ,paste0("Area = ", area))
}
