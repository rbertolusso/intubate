\name{ntbt_oneway}
\alias{ntbt_oneway}
\title{Interface to oneway for data science pipelines.}
\description{
  \code{ntbt_oneway} is an interface to \code{lattice::\link{oneway}}
  (Fit One-way Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_oneway(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{oneway}}.}
}
\details{
  ntbt_oneway calls \code{oneway}.
}
\value{
  Object returned by \code{\link{oneway}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{oneway}}
}
\examples{
library(lattice)

## Original function to interface
fit <- oneway(height ~ voice.part, data = singer, spread = 1)
rfs(fit, aspect = 1)

## The interface reverses the order of data and formula
fit <- ntbt_oneway(data = singer, height ~ voice.part, spread = 1)
rfs(fit, aspect = 1)

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_oneway(height ~ voice.part, spread = 1) \%>\%
  rfs(aspect = 1)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_oneway}
\keyword{oneway}
