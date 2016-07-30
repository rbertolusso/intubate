\name{ntbt_mvr}
\alias{ntbt_mvr}
\title{Interface to mvr for data science pipelines.}
\description{
  \code{ntbt_mvr} is an interface to \code{pls::\link{mvr}}
  (Partial Least Squares and Principal Component Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_mvr(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{mvr}}.}
}
\details{
  ntbt_mvr reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{mvr}.
}
\value{
  Object returned by mvr function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{mvr}}
}
\examples{
library(pls)

## Original function to interface
yarn.mvr <- mvr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV",
                method = "oscorespls")
summary(yarn.mvr)

## The interface reverses the order of data and formula
yarn.mvr <- ntbt_mvr(yarn, density ~ NIR, ncomp = 6, validation = "CV",
                     method = "oscorespls")
summary(yarn.mvr)

## so it can be used easily in a pipeline.
library(magrittr)
yarn \%>\%
  ntbt_mvr(density ~ NIR, ncomp = 6, validation = "CV",
           method = "oscorespls") \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_mvr}
\keyword{mvr}
