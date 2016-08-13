\name{leaps}
\alias{ntbt_regsubsets}
\title{Interfaces for leaps package for data science pipelines.}
\description{
  Interfaces to \code{leaps} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_regsubsets(data, ...)
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
\seealso{
  \code{regsubsets}
}
\examples{
\dontrun{
library(intubate)
library(magrittr)
library(leaps)

## Original function to interface
fit <- regsubsets(Fertility ~ ., data = swiss, nbest = 2)
summary(fit)

## The interface reverses the order of data and formula
fit <- ntbt_regsubsets(data = swiss, Fertility ~ ., nbest = 2)
summary(fit)

## so it can be used easily in a pipeline.
swiss \%>\%
  ntbt_regsubsets(Fertility ~ ., nbest = 2) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{leaps}
\keyword{regsubsets}
