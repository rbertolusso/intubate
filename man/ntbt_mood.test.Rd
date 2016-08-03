\name{ntbt_mood.test}
\alias{ntbt_mood.test}
\title{Interface to mood.test for data science pipelines.}
\description{
  \code{ntbt_mood.test} is an interface to \code{stats::\link{mood.test}}
  (Mood Two-Sample Test of Scale) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_mood.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{mood.test}}.}
}
\details{
  ntbt_mood.test calls \code{mood.test}.
}
\value{
  Object returned by \code{\link{mood.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{mood.test}}
}
\examples{
## Original function to interface
mood.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_mood.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_mood.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_mood.test}
\keyword{mood.test}
