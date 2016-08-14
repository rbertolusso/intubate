\name{pscl}
\alias{ntbt_hurdle}
\alias{ntbt_zeroinfl}
\title{Interfaces for pscl package for data science pipelines.}
\description{
  Interfaces to \code{pscl} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_hurdle(data, ...)
ntbt_zeroinfl(data, ...)
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
library(pscl)


## ntbt_hurdle: Hurdle Models for Count Data Regression
data("bioChemists", package = "pscl")

## Original function to interface
hurdle(art ~ ., data = bioChemists)

## The interface puts data as first parameter
ntbt_hurdle(bioChemists, art ~ .)

## so it can be used easily in a pipeline.
bioChemists \%>\%
  ntbt_hurdle(art ~ .)


## ntbt_zeroinfl: Zero-inflated Count Data Regression
## Original function to interface
zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")

## The interface puts data as first parameter
ntbt_zeroinfl(bioChemists, art ~ . | 1, dist = "negbin")

## so it can be used easily in a pipeline.
bioChemists \%>\%
  ntbt_zeroinfl(art ~ . | 1, dist = "negbin")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{pscl}
\keyword{hurdle}
\keyword{zeroinfl}
