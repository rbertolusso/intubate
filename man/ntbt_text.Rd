\name{ntbt_text}
\alias{ntbt_text}
\title{Interface to text for data science pipelines.}
\description{
  \code{ntbt_text} is an interface to \code{graphics::\link{text}}
  (Formula Notation for Scatterplots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_text(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{text}}.}
}
\details{
  ntbt_text calls \code{text}.
}
\value{
  \code{data}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{text}}
}
\examples{
data <- within(warpbreaks, {
    time <- seq_along(breaks)
    W.T <- wool:tension
})

## Original function to interface
plot(breaks ~ time, data, type = "b")
text(breaks ~ time, data, label = W.T, col = 1 + as.integer(wool))

## The interface reverses the order of data and formula
ntbt_plot(data, breaks ~ time, type = "b")
ntbt_text(data, breaks ~ time, label = W.T, col = 1 + as.integer(wool))

## so it can be used easily in a pipeline.
library(magrittr)
data \%>\%
  ntbt_plot(breaks ~ time, type = "b") \%>\%
  ntbt_text(breaks ~ time, label = W.T, col = 1 + as.integer(wool)) \%>\%
  head()          ## Yes! We still have the data to do what we want!

}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_text}
\keyword{text}
