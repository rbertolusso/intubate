\name{ntbt_qqmath}
\alias{ntbt_qqmath}
\title{Interface to qqmath for data science pipelines.}
\description{
  \code{ntbt_qqmath} is an interface to \code{lattice::\link{qqmath}}
  (Q-Q Plot with Theoretical Distribution) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_qqmath(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{qqmath}}.}
}
\details{
  ntbt_qqmath calls \code{qqmath}.
}
\value{
  Object returned by \code{\link{qqmath}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{qqmath}}
}
\examples{
library(lattice)

## Original function to interface
qqmath(~ height | voice.part, data = singer, aspect = "xy",
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })

## The interface reverses the order of data and formula
ntbt_qqmath(data = singer, ~ height | voice.part, aspect = "xy",
            prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
              panel.qqmathline(x, ...)
              panel.qqmath(x, ...)
            })

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_qqmath(~ height | voice.part, aspect = "xy",
              prepanel = prepanel.qqmathline,
              panel = function(x, ...) {
                panel.qqmathline(x, ...)
                panel.qqmath(x, ...)
              })
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_qqmath}
\keyword{qqmath}
