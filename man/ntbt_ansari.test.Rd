\name{ntbt_ansari.test}
\alias{ntbt_ansari.test}
\title{Interface to ansari.test for data science pipelines.}
\description{
  \code{ntbt_ansari.test} is an interface to \code{stats::\link{ansari.test}}
  (Ansari-Bradley Test) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_ansari.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{ansari.test}}.}
}
\details{
  ntbt_ansari.test calls \code{ansari.test}.
}
\value{
  Object returned by \code{\link{ansari.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{ansari.test}}
}
\examples{
## Original function to interface
ansari.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_ansari.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_ansari.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_ansari.test}
\keyword{ansari.test}
