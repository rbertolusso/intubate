\name{ntbt_bartlett.test}
\alias{ntbt_bartlett.test}
\title{Interface to bartlett.test for data science pipelines.}
\description{
  \code{ntbt_bartlett.test} is an interface to \code{stats::\link{bartlett.test}}
  (Bartlett Test of Homogeneity of Variances) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_bartlett.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{bartlett.test}}.}
}
\details{
  ntbt_bartlett.test calls \code{bartlett.test}.
}
\value{
  Object returned by \code{\link{bartlett.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{bartlett.test}}
}
\examples{
## Original function to interface
bartlett.test(count ~ spray, data = InsectSprays)

## The interface reverses the order of data and formula
ntbt_bartlett.test(data = InsectSprays, count ~ spray)

## so it can be used easily in a pipeline.
library(magrittr)
InsectSprays \%>\%
  ntbt_bartlett.test(count ~ spray)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_bartlett.test}
\keyword{bartlett.test}
