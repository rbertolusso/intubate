\name{ntbt_stripplot}
\alias{ntbt_stripplot}
\title{Interface to stripplot for data science pipelines.}
\description{
  \code{ntbt_stripplot} is an interface to \code{lattice::\link{stripplot}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_stripplot(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{stripplot}}.}
}
\details{
  ntbt_stripplot reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{stripplot}.
}
\value{
  Object returned by \code{\link{stripplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{stripplot}}
}
\examples{
library(lattice)

## Original function to interface
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")

## The interface reverses the order of data and formula
ntbt_stripplot(data = singer, voice.part ~ jitter(height), aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_stripplot(voice.part ~ jitter(height), aspect = 1,
                 jitter.data = TRUE, xlab = "Height (inches)")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_stripplot}
\keyword{stripplot}
