\name{ntbt_multinom}
\alias{ntbt_multinom}
\title{Interface to multinom for data science pipelines.}
\description{
  \code{ntbt_multinom} is an interface to \code{nnet::\link{multinom}}
  (Fit Neural Networks) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_multinom(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{multinom}}.}
}
\details{
  ntbt_multinom calls \code{multinom}.
}
\value{
  Object returned by multinom function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{multinom}}
}
\examples{
library(nnet)

options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)

## Original function to interface
multinom(low ~ ., bwt)

## The interface reverses the order of data and formula
ntbt_multinom(bwt, low ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
bwt \%>\%
  ntbt_multinom(low ~ .)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_multinom}
\keyword{multinom}
