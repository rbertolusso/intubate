\name{mhurdle}
\alias{ntbt_mhurdle}
\title{Interfaces for mhurdle package for data science pipelines.}
\description{
  Interfaces to \code{mhurdle} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_mhurdle(data, ...)
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
library(mhurdle)


## ntbt_mhurdle: Estimation of limited dependent variable models
data("Interview", package = "mhurdle")

## Original function to interface
mhurdle(vacations ~ car + size | linc + linc2 | 0, Interview,
        dist = "ln", h2 = TRUE, method = "bfgs")

## The interface puts data as first parameter
ntbt_mhurdle(Interview, vacations ~ car + size | linc + linc2 | 0,
             dist = "ln", h2 = TRUE, method = "bfgs")

## so it can be used easily in a pipeline.
Interview \%>\%
  ntbt_mhurdle(vacations ~ car + size | linc + linc2 | 0,
               dist = "ln", h2 = TRUE, method = "bfgs")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{mhurdle}
