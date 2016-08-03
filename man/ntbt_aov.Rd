\name{ntbt_aov}
\alias{ntbt_aov}
\title{Interface to aov for data science pipelines.}
\description{
  \code{ntbt_aov} is an interface to \code{stats::\link{aov}}
  (Fit an Analysis of Variance Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_aov(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{aov}}.}
}
\details{
  ntbt_aov calls \code{aov}.
}
\value{
  Object returned by \code{\link{aov}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{aov}}
}
\examples{
## Original function to interface
aov(yield ~ block + N * P + K, npk)

## The interface reverses the order of data and formula
ntbt_aov(npk, yield ~ block + N * P + K)

## so it can be used easily in a pipeline.
library(magrittr)
npk \%>\%
  ntbt_aov(yield ~ block + N * P + K)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_aov}
\keyword{aov}
