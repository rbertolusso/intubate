\name{ntbt_t.test}
\alias{ntbt_t.test}
\title{Interface to t.test for data science pipelines.}
\description{
  \code{ntbt_t.test} is an interface to \code{stats::\link{t.test}}
  (Student's t-Test) so it can be used
  in a pipeline as implemented by \code{magrittr}.}
\usage{
ntbt_t.test(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{t.test}}.}
}
\details{
  ntbt_t.test reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{t.test}.
}
\value{
  Object returned by \code{\link{t.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{t.test}}
}
\examples{
## Original function to interface
t.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_t.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_t.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_t.test}
\keyword{t.test}
