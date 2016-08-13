\name{mgcv}
\alias{ntbt_bam}
\alias{ntbt_gamm}
\title{Interfaces for mgcv package for data science pipelines.}
\description{
  Interfaces to \code{mgcv} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bam(data, ...)
ntbt_gamm(data, ...)
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
library(mgcv)

## ntbt_bam: Generalized additive models for very large datasets
set.seed(3)
dat <- gamSim(1,n=25000,dist="normal",scale=20)
bs <- "cr"
k <- 12

## Original function to interface
bam(y ~ s(x0, bs=bs) + s(x1, bs=bs) + s(x2, bs=bs, k=k) + s(x3, bs=bs), data = dat)

## The interface puts data as first parameter
ntbt_bam(dat, y ~ s(x0, bs=bs) + s(x1, bs=bs) + s(x2, bs=bs, k=k) + s(x3, bs=bs))

## so it can be used easily in a pipeline.
dat \%>\%
  ntbt_bam(y ~ s(x0, bs=bs) + s(x1, bs=bs) + s(x2, bs=bs, k=k) + s(x3, bs=bs))


## ntbt_gam: Generalized additive models with integrated smoothness estimation
set.seed(2) ## simulate some data... 
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
## Original function to interface
gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

## The interface puts data as first parameter
ntbt_gam(dat, y ~ s(x0) + s(x1) + s(x2) + s(x3))

## so it can be used easily in a pipeline.
dat \%>\%
  ntbt_gam(y ~ s(x0) + s(x1) + s(x2) + s(x3))


## ntbt_gamm: Generalized Additive Mixed Models
set.seed(0)
dat <- gamSim(1, n = 200, scale = 2)

## Original function to interface
gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

## The interface puts data as first parameter
ntbt_gamm(dat, y ~ s(x0) + s(x1) + s(x2) + s(x3))

## so it can be used easily in a pipeline.
dat \%>\%
  ntbt_gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{mgcv}
\keyword{bam}
\keyword{gam}
\keyword{gamm}
