#' Logarithm of L, evaluated at specific mu and eta
#' 
#' @param mu a numeric entry with the value of mu where the expression will be evaluated
#' @param eta a numeric entry with the value of eta where the expression will be evaluated
#' @param y a vector of length n (n=number of studies) representing the effect (standardized mean difference)
#' @param sig a vector of length n (n=number of studies) representing the corresponding sampling standard deviation sqrt(variance)
#' @param sig_prior a numeric entry representing the standard deviation's prior; default: 2
#' @param Z a numeric entry representing the Z value; default: 1.96
#' @return the value of L evaluated at mu and eta
#' @author Aldo Cordova-Palomera
#' @details
#' This is an auxiliary function to calculate the L distribution
#' in \code{mass}, which is called to compute the Bayes Factor by \code{BFbias}
#' @seealso \code{mass}, \code{BFbias}
#' @importFrom stats pnorm
#' @export
#' @examples
#' library(metafor)
#' dat <- get(data(dat.bangertdrowns2004))
#' logL(0, 1, dat$yi, sqrt(dat$vi))

logL <-
function(mu, eta, y, sig, sig_prior=2, Z=1.96){
  logdensity <- logL0(mu=mu, y=y, sig=sig, sig_prior=sig_prior)
  SS <- (abs(y) - sig*Z) > 0 #SS: Statistically significant
norm_f <- function(mu, sig, Z=1.96) {1/(1 -pnorm(sig*Z, mean=mu, sd=sig) +pnorm(-sig*Z, mean=mu, sd=sig))}# Normalization constant: f(sig)
  f <- norm_f(mu=mu, sig=sig[SS], Z=Z)
  modif <- sum(log(eta + (1-eta)*f )) + sum(SS==F)*log(eta)
  logdensity + modif}
