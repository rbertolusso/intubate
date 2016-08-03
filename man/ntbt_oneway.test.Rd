\name{ntbt_oneway.test}
\alias{ntbt_oneway.test}
\title{Interface to oneway.test for data science pipelines.}
\description{
  \code{ntbt_oneway.test} is an interface to \code{stats::\link{oneway.test}}
  (Test for Equal Means in a One-Way Layout) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_oneway.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{oneway.test}}.}
}
\details{
  ntbt_oneway.test calls \code{oneway.test}.
}
\value{
  Object returned by \code{\link{oneway.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{oneway.test}}
}
\examples{
## Original function to interface
oneway.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_oneway.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_oneway.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_oneway.test}
\keyword{oneway.test}
