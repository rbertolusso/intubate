\name{forecast}
\alias{ntbt_tslm}
\title{Interfaces for forecast package for data science pipelines.}
\description{
  Interfaces to \code{forecast} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_tslm(data, ...)
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
library(forecast)

## ntbt_tslm: Fit a linear model with time series components
dta <- data.frame(y <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12))

## Original function to interface
tslm(y ~ trend + season, data = dta)

## The interface puts data as first parameter
ntbt_tslm(dta, y ~ trend + season)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_tslm(y ~ trend + season)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{forecast}
\keyword{tslm}
