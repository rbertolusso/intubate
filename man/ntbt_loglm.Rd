\name{ntbt_loglm}
\alias{ntbt_loglm}
\title{Interface to loglm for data science pipelines.}
\description{
  \code{ntbt_loglm} is an interface to \code{MASS::\link{loglm}}
  (Fit Log-Linear Models by Iterative Proportional Scaling) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_loglm(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{loglm}}.}
}
\details{
  ntbt_loglm reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{loglm}}.
}
\value{
  Object returned by \code{\link{loglm}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{loglm}}
}
\examples{
library(MASS)

## Original function to interface
xtCars93 <- xtabs(~ Type + Origin, Cars93)
loglm(~ Type + Origin, xtCars93)

## The interface reverses the order of data and formula
xtCars93 <- ntbt_xtabs(Cars93, ~ Type + Origin)
ntbt_loglm(xtCars93, ~ Type + Origin)

## so it can be used easily in a pipeline.
library(magrittr)
Cars93 \%>\%
  ntbt_xtabs(~ Type + Origin) \%>\%
  ntbt_loglm(~ Type + Origin)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_loglm}
\keyword{loglm}
