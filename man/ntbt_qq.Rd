\name{ntbt_qq}
\alias{ntbt_qq}
\title{Interface to qq for data science pipelines.}
\description{
  \code{ntbt_qq} is an interface to \code{lattice::\link{qq}}
  (Level Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_qq(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{qq}}.}
}
\details{
  ntbt_qq calls \code{qq}.
}
\value{
  Object returned by \code{\link{qq}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{qq}}
}
\examples{
library(lattice)

## Original function to interface
qq(voice.part ~ height, data = singer, aspect = 1,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

## The interface reverses the order of data and formula
ntbt_qq(data = singer, voice.part ~ height, aspect = 1,
        subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_qq(voice.part ~ height, aspect = 1,
          subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_qq}
\keyword{qq}
