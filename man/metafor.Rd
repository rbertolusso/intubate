\name{metafor}
\alias{ntbt_escalc}
\alias{ntbt_rma}
\title{Interfaces for metafor package for data science pipelines.}
\description{
  Interfaces to \code{metafor} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_escalc(data, ...)
ntbt_rma(data, ...)
}
\arguments{
  \item{data}{data frame, tibble, list, ...}
  \item{...}{Other arguments passed to the corresponding interfaced function.}
}
\details{
  Interfaces call their corresponding interfaced function.
}
\value{
  Object returned by interfaced function.
}
\author{
Roberto Bertolusso
}
\examples{
\dontrun{
library(intubate)
library(magrittr)
library(metafor)


## ntbt_escalc: Calculate Effect Sizes and Outcome Measures
data(dat.bcg)
dat.long <- to.long(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
                    data = dat.bcg, append = FALSE, vlong = TRUE)

## Original function to interface
dat <- escalc(measure="RR", outcome ~ group | study, weights = freq, data = dat.long)
rma(yi, vi, data = dat)

## The interface puts data as first parameter
dat <- ntbt_escalc(dat.long, measure="RR", outcome ~ group | study, weights = freq)
ntbt_rma(dat, yi, vi)

## so it can be used easily in a pipeline.
dat.long \%>\%
  ntbt_escalc(measure="RR", outcome ~ group | study, weights = freq) \%>\%
  ntbt_rma(yi, vi)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{metafor}
\keyword{escalc}
\keyword{rma}
