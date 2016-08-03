\name{ntbt_lqs}
\alias{ntbt_lqs}
\title{Interface to lqs for data science pipelines.}
\description{
  \code{ntbt_lqs} is an interface to \code{MASS::\link{lqs}}
  (Resistant Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lqs(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{lqs}}.}
}
\details{
  ntbt_lqs calls \code{\link{lqs}}.
}
\value{
  Object returned by \code{\link{lqs}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lqs}}
}
\examples{
library(MASS)

## Original function to interface
set.seed(123) # make reproducible
lqs(stack.loss ~ ., data = stackloss)

## The interface reverses the order of data and formula
set.seed(123) # make reproducible
ntbt_lqs(data = stackloss, stack.loss ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
set.seed(123) # make reproducible
stackloss \%>\%
  ntbt_lqs(stack.loss ~ .)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lqs}
\keyword{lqs}
