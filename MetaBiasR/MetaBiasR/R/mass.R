#' Value of the definite (two-dimensional) integral of L, 
#' evaluated around the distribution's mean (for mu), 
#' and between 0 and 1 (for eta)
#' 
#' @param y a vector of length n (n=number of studies) representing the effect (standardized mean difference)
#' @param sig a vector of length n (n=number of studies) representing the corresponding sampling standard deviation sqrt(variance)
#' @param sig_prior a numeric entry representing the standard deviation's prior; default: 2
#' @param Z a numeric entry representing the Z value; default: 1.96
#' @return the value of the definite (two-dimensional) integral of the L distribution, 
#' evaluated around the distribution's mean (+-10 standard deviations, for mu), 
#' and between 0 and 1 (for eta)
#' @author Aldo Cordova-Palomera
#' @details
#' This is an auxiliary function to obtain the definite integral of L, 
#' evaluated around the distribution's mean (for mu) and between 0 and 1 for eta. 
#' It calls the \code{logL} function, and solves the integral using \code{integrate} 
#' from the \code{cubature} package. 
#' The output is later called by \code{BFbias} to compute the Bayes Factor. 
#' @seealso \code{logL}, \code{BFbias}
#' @importFrom cubature adaptIntegrate
#' @export
#' @examples
#' library(metafor)
#' dat <- get(data(dat.bangertdrowns2004))
#' mass(dat$yi, sqrt(dat$vi))

mass <-
function(y, sig, sig_prior=2, Z=1.96){
  Fn <- function(arg1, arg2) exp(logL(arg1, arg2, y=y, sig=sig, sig_prior=sig_prior, Z=Z))
  Fn2 <- function(x) Fn(x[1], x[2])#"arg1" stands for "mu"; "arg2" stands for "eta"
a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
b <- -sum(y/sig**2)
var_ <- 1 / (2*a)
mean_ <- -var_*b
sig_ <- sqrt(var_)
  ans <- cubature::adaptIntegrate(Fn2, c(mean_-10*sig_, 0), c(mean_+10*sig_, 1))$integral
  ans}
