\name{mnlogit}
\alias{ntbt_mnlogit}
\title{Interfaces for mnlogit package for data science pipelines.}
\description{
  Interfaces to \code{mnlogit} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_mnlogit(data, ...)
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
library(mnlogit)


## ntbt_mnlogit: Fast estimation of multinomial logit models
library(mnlogit)
data(Fish, package = "mnlogit")

## Original function to interface
mnlogit(mode ~ price | income | catch, Fish, ncores = 2)

## The interface puts data as first parameter
ntbt_mnlogit(Fish, mode ~ price | income | catch, ncores = 2)

## so it can be used easily in a pipeline.
Fish \%>\%
  ntbt_mnlogit(mode ~ price | income | catch, ncores = 2)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{mnlogit}
