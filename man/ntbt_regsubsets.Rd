\name{ntbt_regsubsets}
\alias{ntbt_regsubsets}
\title{Interface to regsubsets for data science pipelines.}
\description{
  \code{ntbt_regsubsets} is an interface to \code{leaps::\link{regsubsets}}
  (Functions for Model Selection, in \code{leaps}) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_regsubsets(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{regsubsets}}.}
}
\details{
  ntbt_regsubsets calls \code{regsubsets}.
}
\value{
  Object returned by \code{\link{regsubsets}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{regsubsets}}
}
\examples{
library(leaps)

## Original function to interface
fit <- regsubsets(Fertility ~ ., data = swiss, nbest = 2)
summary(fit)

## The interface reverses the order of data and formula
fit <- ntbt_regsubsets(data = swiss, Fertility ~ ., nbest = 2)
summary(fit)

## so it can be used easily in a pipeline.
library(magrittr)
swiss \%>\%
  ntbt_regsubsets(Fertility ~ ., nbest = 2) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_regsubsets}
\keyword{regsubsets}
