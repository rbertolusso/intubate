\name{ivfixed}
\alias{ntbt_ivFixed}
\title{Interfaces for ivfixed package for data science pipelines.}
\description{
  Interfaces to \code{ivfixed} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_ivFixed(data, ...)
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
library(ivfixed)


## ntbt_ivFixed: Fit an Instrumental least square dummy variable model
pib <- as.matrix(c(12,3,4,0.4,0.7,5,0.7,0.3,
                   0.6,89,7,8,45,7,4,5,0.5,5),nrows=18,ncols=1)
tir <- as.matrix(c(12,0.3,4,0.4,7,12,3.0,6.0,45,
                   7.0,0.8,44,65,23,4,6,76,9),nrows=18,ncols=1)
inf <- as.matrix(c(1.2,3.6,44,1.4,0.78,54,0.34,0.66,
                   12,0.7,8.0,12,65,43,5,76,65,8),nrows=18,ncols=1)
npl <- as.matrix(c(0.2,3.8,14,2.4,1.7,43,0.2,0.5,23,
                   7.8,88,36,65,3,44,65,7,34),nrows=18,ncols=1)
mdata <- data.frame(p = pib, t = tir, int = inf, np = npl)

## Original function to interface
ivFixed(t ~ p + int | p + np, mdata, n = 6, t = 3)

## The interface puts data as first parameter
ntbt_ivFixed(mdata, t ~ p + int | p + np, n = 6, t = 3)

## so it can be used easily in a pipeline.
mdata \%>\%
  ntbt_ivFixed(t ~ p + int | p + np, n = 6, t = 3)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ivfixed}
\keyword{ivFixed}
