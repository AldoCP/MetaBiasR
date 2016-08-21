#' Computes the Bayes Factor for bias
#' 
#' @param y a vector of length n (n=number of studies) representing the effect (standardized mean difference)
#' @param sig a vector of length n (n=number of studies) representing the corresponding sampling standard deviation sqrt(variance)
#' @param sig_prior a numeric entry representing the standard deviation's prior; default: 2
#' @param Z a numeric entry representing the Z value; default: 1.96
#' @return the value of the Bayes Factor
#' @author Aldo Cordova-Palomera
#' @details
#' Computes the Bayes Factor for bias in a Meta-Analysis
#' @seealso \code{logL0} \code{logL} \code{nullmass} \code{lmass}
#' @export
#' @examples
#' library(metafor)
#' dat <- get(data(dat.bangertdrowns2004))
#' BFbias(dat$yi, sqrt(dat$vi))

BFbias <-
function(y, sig, sig_prior=2, Z=1.96){
ans <- mass(y, sig, sig_prior=2, Z=1.96)/nullmass(y, sig, sig_prior=2)
ans}
