\name{ntbt_rlm}
\alias{ntbt_rlm}
\title{Interface to rlm for data science pipelines.}
\description{
  \code{ntbt_rlm} is an interface to \code{MASS::\link{rlm}}
  (Robust Fitting of Linear Models) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_rlm(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{rlm}}.}
}
\details{
  ntbt_rlm calls \code{\link{rlm}}.
}
\value{
  Object returned by \code{\link{rlm}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{rlm}}
}
\examples{
library(MASS)

## Original function to interface
rlm(stack.loss ~ ., stackloss)

## The interface reverses the order of data and formula
ntbt_rlm(stackloss, stack.loss ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
stackloss \%>\%
  ntbt_rlm(stack.loss ~ .) \%>\%
  summary()
  
stackloss \%>\%
  ntbt_rlm(stack.loss ~ ., psi = psi.hampel, init = "lts") \%>\%
  summary()

stackloss \%>\%
  ntbt_rlm(stack.loss ~ ., psi = psi.bisquare) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_rlm}
\keyword{rlm}
