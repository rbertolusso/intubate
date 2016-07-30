\name{ntbt_lm.gls}
\alias{ntbt_lm.gls}
\title{Interface to lm.gls for data science pipelines.}
\description{
  \code{ntbt_lm.gls} is an interface to \code{MASS::\link{lm.gls}}
  (Fit Linear Models by Generalized Least Squares) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lm.gls(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lm.gls}}.}
}
\details{
  ntbt_lm.gls reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{lm.gls}}.
}
\value{
  Object returned by \code{\link{lm.gls}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lm.gls}}
}
\examples{
library(MASS)

## Original function to interface
lm.gls(conc ~ uptake, CO2, W = diag(nrow(CO2)))

## The interface reverses the order of data and formula
ntbt_lm.gls(CO2, conc ~ uptake, W = diag(nrow(CO2)))

## so it can be used easily in a pipeline.
library(magrittr)
CO2 \%>\%
  ntbt_lm.gls(conc ~ uptake, W = diag(nrow(CO2)))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lm.gls}
\keyword{lm.gls}
