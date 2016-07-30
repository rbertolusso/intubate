\name{ntbt_plsr}
\alias{ntbt_plsr}
\title{Interface to plsr for data science pipelines.}
\description{
  \code{ntbt_plsr} is an interface to \code{pls::\link{plsr}}
  (Partial Least Squares Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_plsr(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{plsr}}.}
}
\details{
  ntbt_plsr reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{plsr}.
}
\value{
  Object returned by plsr function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{plsr}}
}
\examples{
library(pls)

## Original function to interface
yarn.plsr <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.plsr)

## The interface reverses the order of data and formula
yarn.plsr <- ntbt_plsr(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.plsr)

## so it can be used easily in a pipeline.
library(magrittr)
yarn \%>\%
  ntbt_plsr(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_plsr}
\keyword{plsr}
