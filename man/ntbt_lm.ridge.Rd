\name{ntbt_lm.ridge}
\alias{ntbt_lm.ridge}
\title{Interface to lm.ridge for data science pipelines.}
\description{
  \code{ntbt_lm.ridge} is an interface to \code{MASS::\link{lm.ridge}}
  (Ridge Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lm.ridge(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lm.ridge}}.}
}
\details{
  ntbt_lm.ridge reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{lm.ridge}}.
}
\value{
  Object returned by \code{\link{lm.ridge}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lm.ridge}}
}
\examples{
library(MASS)

## Original function to interface
lm.ridge(GNP.deflator ~ ., longley)

## The interface reverses the order of data and formula
ntbt_lm.ridge(longley, GNP.deflator ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
longley \%>\%
  ntbt_lm.ridge(GNP.deflator ~ .)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lm.ridge}
\keyword{lm.ridge}
