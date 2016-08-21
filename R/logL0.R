#' Logarithm of L0, evaluated at a specific mu
#' 
#' @param mu a numeric entry giving the value where the expression will be evaluated
#' @param y a vector of length n (n=number of studies) representing the effect (standardized mean difference)
#' @param sig a vector of length n (n=number of studies) representing the corresponding sampling standard deviation sqrt(variance)
#' @param sig_prior a numeric entry representing the standard deviation's prior; default: 2
#' @return the value of L0 evaluated at mu
#' @author Aldo Cordova-Palomera
#' @details
#' This is an auxiliary function to calculate the L0 distribution
#' in \code{nullmass}, which is called to compute the Bayes Factor by \code{BFbias}
#' @seealso \code{nullmass}, \code{BFbias}
#' @export
#' @examples
#' library(metafor)
#' dat <- get(data(dat.bangertdrowns2004))
#' logL0(0, dat$yi, sqrt(dat$vi))

logL0 <-
function(mu, y, sig, sig_prior=2){
  a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
  b <- -sum(y/sig**2)
  c <- 0.5*sum(y**2/sig**2) + sum(log(sig)) + log(sig_prior) + 0.5*(length(y)+1)*log(2*pi)
  (-(a*(mu**2) + b*mu + c)) }
