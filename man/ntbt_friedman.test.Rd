\name{ntbt_friedman.test}
\alias{ntbt_friedman.test}
\title{Interface to friedman.test for data science pipelines.}
\description{
  \code{ntbt_friedman.test} is an interface to \code{stats::\link{friedman.test}}
  (Friedman Rank Sum Test) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_friedman.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{friedman.test}}.}
}
\details{
  ntbt_friedman.test calls \code{friedman.test}.
}
\value{
  Object returned by \code{\link{friedman.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{friedman.test}}
}
\examples{
wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)

## Original function to interface
friedman.test(x ~ w | t, data = wb)

## The interface reverses the order of data and formula
ntbt_friedman.test(data = wb, x ~ w | t)

## so it can be used easily in a pipeline.
library(magrittr)
wb \%>\%
  ntbt_friedman.test(x ~ w | t)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_friedman.test}
\keyword{friedman.test}
