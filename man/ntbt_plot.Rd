\name{ntbt_plot}
\alias{ntbt_plot}
\title{Interface to plot for data science pipelines.}
\description{
  \code{ntbt_plot} is an interface to \code{graphics::\link{plot}}
  (Formula Notation for Scatterplots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_plot(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{plot}}.}
}
\details{
  ntbt_plot reverses the order of \code{data} and \code{formula}, so it can be
  part of a pipeline. It then calls \code{plot}.
}
\value{
  \code{data}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{plot}}
}
\examples{
## Original function to interface
plot(Ozone ~ Wind, data = airquality, pch = as.character(Month))

## The interface reverses the order of data and formula
ntbt_plot(data = airquality, Ozone ~ Wind, pch = as.character(Month))

## so it can be used easily in a pipeline.
library(magrittr)
airquality \%>\%
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month))
  
op <- par(mfrow = c(2,1))
airquality \%>\%  ## Note: we are *not* using the tee operator.
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month)) \%>\%
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month),
            subset = Month != 7) \%>\%
  head()          ## Yes! We still have the data to do what we want!
par(op)

## text.formula() can be very natural:
within(warpbreaks, {
    time <- seq_along(breaks)
    W.T <- wool:tension
}) \%>\%
  ntbt_plot(breaks ~ time, type = "b") \%>\%
  ntbt_text(breaks ~ time, label = W.T, col = 1 + as.integer(wool))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_plot}
\keyword{plot}
