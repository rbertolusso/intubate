\name{ntbt_bwplot}
\alias{ntbt_bwplot}
\title{Interface to bwplot for data science pipelines.}
\description{
  \code{ntbt_bwplot} is an interface to \code{lattice::\link{bwplot}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_bwplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{bwplot}}.}
}
\details{
  ntbt_bwplot calls \code{bwplot}.
}
\value{
  Object returned by \code{\link{bwplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{bwplot}}
}
\examples{
library(lattice)

## Original function to interface
bwplot(voice.part ~ height, data = singer, xlab = "Height (inches)")

## The interface reverses the order of data and formula
ntbt_bwplot(data = singer, voice.part ~ height, xlab = "Height (inches)")

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_bwplot(voice.part ~ height, xlab = "Height (inches)")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_bwplot}
\keyword{bwplot}
