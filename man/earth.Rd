\name{earth}
\alias{ntbt_earth}
\title{Interfaces for earth package for data science pipelines.}
\description{
  Interfaces to \code{earth} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_earth(data, ...)
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
library(earth)

## ntbt_earth: Multivariate Adaptive Regression Splines
## Original function to interface
earth(Volume ~ ., data = trees)

## The interface puts data as first parameter
ntbt_earth(trees, Volume ~ .)

## so it can be used easily in a pipeline.
trees \%>\%
  ntbt_earth(Volume ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{earth}
