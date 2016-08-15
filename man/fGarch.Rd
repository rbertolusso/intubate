\name{fGarch}
\alias{ntbt_garchFit}
\title{Interfaces for fGarch package for data science pipelines.}
\description{
  Interfaces to \code{fGarch} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_garchFit(data, ...)
}
\arguments{
  \item{data}{data frame, tibble, list, ...}
  \item{...}{Other arguments passed to the corresponding interfaced function.}
}
\details{
  Interfaces call their corresponding interfaced function.
}
\value{
  Object returned by interfaced function.
}
\author{
Roberto Bertolusso
}
\examples{
\dontrun{
library(intubate)
library(magrittr)
library(fGarch)


## ntbt_garchFit: Univariate GARCH Time Series Fitting
N <- 200
x.vec <- as.vector(garchSim(garchSpec(rseed = 1985), n = N)[,1])

## Original function to interface
garchFit(~ garch(1,1), data = x.vec, trace = FALSE)

## The interface puts data as first parameter
ntbt_garchFit(data = x.vec, ~ garch(1,1), trace = FALSE)

## so it can be used easily in a pipeline.
x.vec \%>\%
  ntbt_garchFit(~ garch(1,1), trace = FALSE)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{fGarch}
\keyword{garchFit}
