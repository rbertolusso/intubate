\name{ntbt_ftable}
\alias{ntbt_ftable}
\title{Interface to ftable for data science pipelines.}
\description{
  \code{ntbt_ftable} is an interface to \code{stats::\link{ftable}}
  (Flat Contingency Tables) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_ftable(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{ftable}}.}
}
\details{
  ntbt_ftable reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{ftable}}.
}
\value{
  Object returned by \code{\link{ftable}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{ftable}}, \code{\link{ntbt_aggregate}}
}
\examples{
## Original function to interface
x <- ftable(Survived ~ ., data = Titanic)
ftable(Sex ~ Class + Age, data = x)

## The interface reverses the order of data and formula
x <- ntbt_ftable(data = Titanic, Survived ~ .)
ftable(data = x, Sex ~ Class + Age)

## so it can be used easily in a pipeline.
library(magrittr)
Titanic \%>\%
  ntbt_ftable(Survived ~ .)

Titanic \%>\%
  ntbt_ftable(Survived ~ .) \%>\%
  ntbt_ftable(Sex ~ Class + Age)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_aggregate}
\keyword{ntbt_ftable}
\keyword{ftable}
