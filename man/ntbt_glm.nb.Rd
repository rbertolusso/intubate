\name{ntbt_glm.nb}
\alias{ntbt_glm.nb}
\title{Interface to glm.nb for data science pipelines.}
\description{
  \code{ntbt_glm.nb} is an interface to \code{MASS::\link{glm.nb}}
  (Fit a Negative Binomial Generalized Linear Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_glm.nb(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{glm.nb}}.}
}
\details{
  ntbt_glm.nb reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{glm.nb}}.
}
\value{
  Object returned by \code{\link{glm.nb}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{glm.nb}}
}
\examples{
library(MASS)
## Original function to interface
glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)

## The interface reverses the order of data and formula
ntbt_glm.nb(data = quine, Days ~ Sex/(Age + Eth*Lrn))

## so it can be used easily in a pipeline.
library(magrittr)
quine \%>\%
  ntbt_glm.nb(Days ~ Sex/(Age + Eth*Lrn))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_glm.nb}
\keyword{glm.nb}
