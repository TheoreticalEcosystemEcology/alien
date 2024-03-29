% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitGLM.R
\name{fitGLM}
\alias{fitGLM}
\title{Fit direct matching centrality using generalized linear model}
\usage{
fitGLM(data, formula, family = NULL, spRandom = FALSE, ...)
}
\arguments{
\item{data}{an object of the class \code{\link{alienData}}}

\item{formula}{A one-sided formula specifying how the different traits from both sets of species should be used to estimate species interactions.}

\item{family}{The family of the response variable. See \link[stats]{family}, or the choices available.}

\item{spRandom}{Logical. Whether species are used as a random effect. Default is FALSE.}

\item{\dots}{Other parameters passed to \link[stats]{glm}.}
}
\value{
An object with a class alienFit and a class fitGLM.
}
\description{
Fit direct matching centrality model using generalized linear model
}
\details{
This function unfold the adjacency matrix and uses it as the response variable. As explanatory variables, the traits for each sets of species are repeated to match the length of the unfolded adjacency matrix but also the position.

If there are NAs in the adjacency matrix, the function will omit these values in the estimation of the model.

Although not specified by default formula is proposed here, in the ecological literature focusing on modeling species interactions, all variables are considered additively. In addition, quadratic relations are included for quantitative terms. Lastly, interactions between traits are considered across all traits within and across trophic levels. This is different than from the fourth corner approach where interactions is considered solely between traits of different trophic levels.
}
\author{
F. Guillaume Blanchet, Dominique Gravel, Steve Vissault
}
