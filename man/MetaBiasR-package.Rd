\name{MetaBiasR-package}
\alias{MetaBiasR-package}
\alias{MetaBiasR}
\docType{package}
\title{
Assessment of Bias in a Meta-Analysis in a Bayesian framework
}
\description{
Estimates the Bayes Factor for bias in a Meta-Analysis
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{MetaBiasR}
\packageIndices{MetaBiasR}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Aldo Cordova-Palomera

Maintainer: Aldo Cordova-Palomera <a.cordova@ub.edu>
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link[<compute.es>:<compute.es>-package]{<compute.es>}} \code{\link[<metafor>:<metafor>-package]{<metafor>}} 
}
\examples{
library(metafor); dat <- get(data(dat.bangertdrowns2004))
BFbias(dat$yi, sqrt(dat$vi))
}
