\name{ntbt_pcr}
\alias{ntbt_pcr}
\title{Interface to pcr for data science pipelines.}
\description{
  \code{ntbt_pcr} is an interface to \code{pls::\link{pcr}}
  (Principal Component Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_pcr(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{pcr}}.}
}
\details{
  ntbt_pcr calls \code{pcr}.
}
\value{
  Object returned by pcr function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{pcr}}
}
\examples{
library(pls)

## Original function to interface
yarn.pcr <- pcr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.pcr)

## The interface reverses the order of data and formula
yarn.pcr <- ntbt_pcr(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.pcr)

## so it can be used easily in a pipeline.
library(magrittr)
yarn \%>\%
  ntbt_pcr(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_pcr}
\keyword{pcr}
