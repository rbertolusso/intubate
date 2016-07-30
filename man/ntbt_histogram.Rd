\name{ntbt_histogram}
\alias{ntbt_histogram}
\title{Interface to histogram for data science pipelines.}
\description{
  \code{ntbt_histogram} is an interface to \code{lattice::\link{histogram}}
  (Histograms) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_histogram(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{histogram}}.}
}
\details{
  ntbt_histogram reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{histogram}.
}
\value{
  Object returned by \code{\link{histogram}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{histogram}}
}
\examples{
library(lattice)

## Original function to interface
histogram(~ height | voice.part, data = singer,
          xlab = "Height (inches)", type = "density",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm, col = "black",
                              args = list(mean=mean(x),sd=sd(x)))
          })

## The interface reverses the order of data and formula
ntbt_histogram(data = singer, ~ height | voice.part,
               xlab = "Height (inches)", type = "density",
               panel = function(x, ...) {
                 panel.histogram(x, ...)
                 panel.mathdensity(dmath = dnorm, col = "black",
                                   args = list(mean=mean(x),sd=sd(x)))
               })

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_histogram(~ height | voice.part,
                 xlab = "Height (inches)", type = "density",
                 panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
                 })
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_histogram}
\keyword{histogram}
