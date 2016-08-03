\name{ntbt_survfit}
\alias{ntbt_survfit}
\title{Interface to survfit for data science pipelines.}
\description{
  \code{ntbt_survfit} is an interface to \code{survival::\link{survfit}}
  (Compute a Survival Curve for Censored Data) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_survfit(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{survfit}}.}
}
\details{
  ntbt_survfit calls \code{\link{survfit}}.
}
\value{
  Object returned by \code{\link{survfit}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{survfit}}
}
\examples{
library(survival)

## Original function to interface
survfit(Surv(time, status) ~ x, data = aml)

## The interface reverses the order of data and formula
ntbt_survfit(data = aml, Surv(time, status) ~ x)

## so it can be used easily in a pipeline.
library(magrittr)
aml \%>\%
  ntbt_survfit(Surv(time, status) ~ x)

aml \%>\%
  ntbt_survfit(Surv(time, status) ~ x) \%>\%
  plot(lty = 2:3)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_survfit}
\keyword{survfit}
