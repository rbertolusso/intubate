\name{ntbt_replications}
\alias{ntbt_replications}
\title{Interface to replications for data science pipelines.}
\description{
  \code{ntbt_replications} is an interface to \code{stats::\link{replications}}
  (Number of Replications of Terms) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_replications(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{replications}}.}
}
\details{
  ntbt_replications reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{replications}.
}
\value{
  Object returned by \code{\link{replications}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{replications}}
}
\examples{
## From Venables and Ripley (2002) p.165.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
           55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)

## Original function to interface
replications(~ . - yield, npk)

## The interface reverses the order of data and formula
ntbt_replications(npk, ~ . - yield)

## so it can be used easily in a pipeline.
library(magrittr)
npk \%>\%
  ntbt_replications(~ . - yield)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_replications}
\keyword{replications}
