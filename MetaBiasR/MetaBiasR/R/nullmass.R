#' Value of the definite integral of L0, evaluated around the distribution's mean
#' 
#' @param y a vector of length n (n=number of studies) representing the effect (standardized mean difference)
#' @param sig a vector of length n (n=number of studies) representing the corresponding sampling standard deviation sqrt(variance)
#' @param sig_prior a numeric entry representing the standard deviation's prior; default: 2
#' @return the value of the definite integral of the L0 distribution, 
#' evaluated around its mean (+-10 standard deviations)
#' @author Aldo Cordova-Palomera
#' @details
#' This is an auxiliary function to obtain the definite integral of L0, 
#' evaluated around the distribution's mean. It calls the \code{logL0} function, 
#' and performs integration using \code{integrate}. 
#' The output is later called by \code{BFbias} to compute the Bayes Factor. 
#' @seealso \code{logL0}, \code{BFbias}
#' @importFrom stats integrate
#' @export
#' @examples
#' library(metafor)
#' dat <- get(data(dat.bangertdrowns2004))
#' nullmass(dat$yi, sqrt(dat$vi))

nullmass <-
function(y, sig, sig_prior=2){
  Fn0 <- function(arg) exp(logL0(arg, y=y, sig=sig, sig_prior=sig_prior))
a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
b <- -sum(y/sig**2)
var_ <- 1 / (2*a)
mean_ <- -var_*b
sig_ <- sqrt(var_)
  ans <- integrate(Fn0, lower=mean_-10*sig_, upper=mean_+10*sig_)$value
  ans}
