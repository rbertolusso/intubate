\name{pls}
\alias{ntbt_cppls}
\alias{ntbt_mvr}
\alias{ntbt_pcr}
\alias{ntbt_plsr}
\title{Interfaces for pls package for data science pipelines.}
\description{
  Interfaces to \code{pls} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_cppls(data, ...)
ntbt_mvr(data, ...)
ntbt_pcr(data, ...)
ntbt_plsr(data, ...)
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
library(magrittr)
library(pls)

## cppls
## Original function to interface
yarn.cppls <- cppls(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.cppls)

## The interface reverses the order of data and formula
yarn.cppls <- ntbt_cppls(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.cppls)

## so it can be used easily in a pipeline.
yarn \%>\%
  ntbt_cppls(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()

## mvr
## Original function to interface
yarn.mvr <- mvr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV",
                method = "oscorespls")
summary(yarn.mvr)

## The interface reverses the order of data and formula
yarn.mvr <- ntbt_mvr(yarn, density ~ NIR, ncomp = 6, validation = "CV",
                     method = "oscorespls")
summary(yarn.mvr)

## so it can be used easily in a pipeline.
yarn \%>\%
  ntbt_mvr(density ~ NIR, ncomp = 6, validation = "CV",
           method = "oscorespls") \%>\%
  summary()

## pcr
## Original function to interface
yarn.pcr <- pcr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.pcr)

## The interface reverses the order of data and formula
yarn.pcr <- ntbt_pcr(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.pcr)

## so it can be used easily in a pipeline.
yarn \%>\%
  ntbt_pcr(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()

## plsr
## Original function to interface
yarn.plsr <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.plsr)

## The interface reverses the order of data and formula
yarn.plsr <- ntbt_plsr(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.plsr)

## so it can be used easily in a pipeline.
yarn \%>\%
  ntbt_plsr(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{pls}
\keyword{cppls}
\keyword{mvr}
\keyword{pcr}
\keyword{plsr}
