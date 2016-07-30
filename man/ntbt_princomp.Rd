\name{ntbt_princomp}
\alias{ntbt_princomp}
\title{Interface to princomp for data science pipelines.}
\description{
  \code{ntbt_princomp} is an interface to \code{stats::\link{princomp}}
  (Principal Components Analysis) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_princomp(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{princomp}}.}
}
\details{
  ntbt_princomp reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{princomp}.
}
\value{
  Object returned by \code{\link{princomp}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{princomp}}
}
\examples{
## Original function to interface
princomp(~ ., data = USArrests, cor = TRUE)

## The interface reverses the order of data and formula
ntbt_princomp(data = USArrests, ~ ., cor = TRUE)

## so it can be used easily in a pipeline.
library(magrittr)
USArrests \%>\%
  ntbt_princomp(~ ., cor = TRUE)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_princomp}
\keyword{princomp}
