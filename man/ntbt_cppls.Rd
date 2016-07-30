\name{ntbt_cppls}
\alias{ntbt_cppls}
\title{Interface to cppls for data science pipelines.}
\description{
  \code{ntbt_cppls} is an interface to \code{pls::\link{cppls}}
  (Canonical Powered Partial Least Squares Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cppls(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{cppls}}.}
}
\details{
  ntbt_cppls reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{cppls}.
}
\value{
  Object returned by cppls function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{cppls}}
}
\examples{
library(pls)

## Original function to interface
yarn.cppls <- cppls(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
summary(yarn.cppls)

## The interface reverses the order of data and formula
yarn.cppls <- ntbt_cppls(yarn, density ~ NIR, ncomp = 6, validation = "CV")
summary(yarn.cppls)

## so it can be used easily in a pipeline.
library(magrittr)
yarn \%>\%
  ntbt_cppls(density ~ NIR, ncomp = 6, validation = "CV") \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_cppls}
\keyword{cppls}
