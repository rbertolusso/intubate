\name{ntbt_plot}
\alias{ntbt_plot}
\title{Interface to plot for data science pipelines.}
\description{
  \code{ntbt_plot} is an interface to \code{graphics::\link{plot}}
  (Generic X-Y Plotting) that can be used
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
  Object returned by \code{\link{plot}}.
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
airquality \%T>\%  ## Note the tee operator.
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month)) \%>\%
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month),
            subset = Month != 7)
par(op)

}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_plot}
\keyword{plot}
