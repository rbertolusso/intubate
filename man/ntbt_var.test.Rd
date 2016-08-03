\name{ntbt_var.test}
\alias{ntbt_var.test}
\title{Interface to var.test for data science pipelines.}
\description{
  \code{ntbt_var.test} is an interface to \code{stats::\link{var.test}}
  (F Test to Compare Two Variances) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_var.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{var.test}}.}
}
\details{
  ntbt_var.test calls \code{var.test}.
}
\value{
  Object returned by \code{\link{var.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{var.test}}
}
\examples{
## Original function to interface
var.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_var.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_var.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_var.test}
\keyword{var.test}
