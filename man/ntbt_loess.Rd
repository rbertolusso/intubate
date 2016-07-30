\name{ntbt_loess}
\alias{ntbt_loess}
\title{Interface to loess for data science pipelines.}
\description{
  \code{ntbt_loess} is an interface to \code{stats::\link{loess}}
  (Local Polynomial Regression Fitting) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_loess(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{loess}}.}
}
\details{
  ntbt_loess reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{loess}.
}
\value{
  Object returned by \code{\link{loess}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{loess}}
}
\examples{
## Original function to interface
loess(dist ~ speed, cars)

## The interface reverses the order of data and formula
ntbt_loess(cars, dist ~ speed)

## so it can be used easily in a pipeline.
library(magrittr)
cars \%>\%
  ntbt_loess(dist ~ speed)
  
cars \%>\%
  ntbt_loess(dist ~ speed,
             control = loess.control(surface = "direct"))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_loess}
\keyword{loess}
