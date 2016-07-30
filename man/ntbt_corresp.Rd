\name{ntbt_corresp}
\alias{ntbt_corresp}
\title{Interface to corresp for data science pipelines.}
\description{
  \code{ntbt_corresp} is an interface to \code{MASS::\link{corresp}}
  (Simple Correspondence Analysis) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_corresp(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{corresp}}.}
}
\details{
  ntbt_corresp reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{corresp}}.
}
\value{
  Object returned by \code{\link{corresp}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{corresp}}
}
\examples{
library(MASS)
## Original function to interface
corresp(~ Age + Eth, data = quine)

## The interface reverses the order of data and formula
ntbt_corresp(data = quine, ~ Age + Eth)

## so it can be used easily in a pipeline.
library(magrittr)
quine \%>\%
  ntbt_corresp(~ Age + Eth)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_corresp}
\keyword{corresp}
