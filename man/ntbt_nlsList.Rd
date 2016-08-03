\name{ntbt_nlsList}
\alias{ntbt_nlsList}
\title{Interface to nlsList for data science pipelines.}
\description{
  \code{ntbt_nlsList} is an interface to \code{nlme::\link{nlsList}}
  (List of nls Objects with a Common Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_nlsList(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{nlsList}}.}
}
\details{
  ntbt_nlsList calls \code{nlsList}.
}
\value{
  Object returned by \code{\link{nlsList}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{nlsList}}
}
\examples{
library(nlme)

## Original function to interface
fm1 <- nlsList(uptake ~ SSasympOff(conc, Asym, lrc, c0),
               data = CO2, start = c(Asym = 30, lrc = -4.5, c0 = 52))
summary(fm1)

## The interface reverses the order of data and formula
fm1 <- ntbt_nlsList(data = CO2, uptake ~ SSasympOff(conc, Asym, lrc, c0),
                    start = c(Asym = 30, lrc = -4.5, c0 = 52))
summary(fm1)

## so it can be used easily in a pipeline.
library(magrittr)
CO2 \%>\%
  ntbt_nlsList(uptake ~ SSasympOff(conc, Asym, lrc, c0),
               start = c(Asym = 30, lrc = -4.5, c0 = 52)) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_nlsList}
\keyword{nlsList}
