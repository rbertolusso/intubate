\name{gee}
\alias{ntbt_gee}
\title{Interfaces for gee package for data science pipelines.}
\description{
  Interfaces to \code{gee} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gee(data, ...)
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
library(gee)

## ntbt_gee: solve a Generalized Estimation Equation Model
data(warpbreaks)

## Original function to interface
gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable")

## The interface puts data as first parameter
ntbt_gee(warpbreaks, breaks ~ tension, id=wool, corstr="exchangeable")

## so it can be used easily in a pipeline.
warpbreaks \%>\%
  ntbt_gee(breaks ~ tension, id=wool, corstr="exchangeable")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gee}
