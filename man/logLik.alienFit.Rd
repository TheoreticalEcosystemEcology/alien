% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logLik.alienFit.R
\name{logLik.alienFit}
\alias{logLik.alienFit}
\title{Extract Log-Likelihood for \code{alienfit} object}
\usage{
\method{logLik}{alienFit}(object, ...)
}
\arguments{
\item{object}{an object of class \code{alienfit}.}

\item{\dots}{Some methods for this function require additional arguments.}
}
\value{
A numerical value presenting a log-likelihood.
}
\description{
Calculate a Log-Likelihood for and object class \code{alienfit}
}
\details{
For objects of class \code{fitKNN}, log-likelihoods are only available for presence-absence data. An error will be sent otherwise.
}
\author{
F. Guillaume Blanchet and Dominique Gravel
}
\keyword{univar}
