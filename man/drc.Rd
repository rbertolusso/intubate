\name{drc}
\alias{ntbt_drm}
\title{Interfaces for drc package for data science pipelines.}
\description{
  Interfaces to \code{drc} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_drm(data, ...)
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
library(drc)

## ntbt_drm: Fitting dose-response models
## Original function to interface
drm(rootl ~ conc, data = ryegrass, fct = W2.4())

## The interface puts data as first parameter
ntbt_drm(ryegrass, rootl ~ conc, fct = W2.4())

## so it can be used easily in a pipeline.
ryegrass \%>\%
  ntbt_drm(rootl ~ conc, fct = W2.4())
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{drc}
\keyword{drm}
