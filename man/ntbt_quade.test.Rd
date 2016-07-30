\name{ntbt_quade.test}
\alias{ntbt_quade.test}
\title{Interface to quade.test for data science pipelines.}
\description{
  \code{ntbt_quade.test} is an interface to \code{stats::\link{quade.test}}
  (Quade Test) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_quade.test(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{quade.test}}.}
}
\details{
  ntbt_quade.test reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{quade.test}.
}
\value{
  Object returned by \code{\link{quade.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{quade.test}}
}
\examples{
wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)

## Original function to interface
quade.test(x ~ w | t, data = wb)

## The interface reverses the order of data and formula
ntbt_quade.test(data = wb, x ~ w | t)

## so it can be used easily in a pipeline.
library(magrittr)
wb \%>\%
  ntbt_quade.test(x ~ w | t)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_quade.test}
\keyword{quade.test}
