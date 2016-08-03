\name{ntbt_ppr}
\alias{ntbt_ppr}
\title{Interface to ppr for data science pipelines.}
\description{
  \code{ntbt_ppr} is an interface to \code{stats::\link{ppr}}
  (Projection Pursuit Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_ppr(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{ppr}}.}
}
\details{
  ntbt_ppr calls \code{ppr}.
}
\value{
  Object returned by \code{\link{ppr}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{ppr}}
}
\examples{
## Original function to interface
ppr(log(perm) ~ area + peri + shape, data = rock,
    nterms = 2, max.terms = 5)

## The interface reverses the order of data and formula
ntbt_ppr(data = rock, log(perm) ~ area + peri + shape,
         nterms = 2, max.terms = 5)

## so it can be used easily in a pipeline.
library(magrittr)
rock \%>\%
  ntbt_ppr(log(perm) ~ area + peri + shape,
           nterms = 2, max.terms = 5)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_ppr}
\keyword{ppr}
