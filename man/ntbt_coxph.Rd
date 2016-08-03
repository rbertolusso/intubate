\name{ntbt_coxph}
\alias{ntbt_coxph}
\title{Interface to coxph for data science pipelines.}
\description{
  \code{ntbt_coxph} is an interface to \code{survival::\link{coxph}}
  (Fit Proportional Hazards Regression Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_coxph(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{coxph}}.}
}
\details{
  ntbt_coxph calls \code{\link{coxph}}.
}
\value{
  Object returned by \code{\link{coxph}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{coxph}}
}
\examples{
library(survival)

## Original function to interface
vet2 <- survSplit(Surv(time, status) ~., veteran,
                  cut = c(60, 120), episode = "timegroup")
coxph(Surv(tstart, time, status) ~ karno*strata(timegroup) +
                age + trt, data = vet2)

## The interface reverses the order of data and formula
vet2 <- ntbt_survSplit(veteran, Surv(time, status) ~.,
                       cut = c(60, 120), episode = "timegroup")
ntbt_coxph(data = vet2, Surv(tstart, time, status) ~
              karno*strata(timegroup) + age + trt)

## so it can be used easily in a pipeline.
library(magrittr)
veteran \%>\%
  ntbt_survSplit(Surv(time, status) ~.,
                 cut = c(60, 120), episode = "timegroup") \%>\%
  ntbt_coxph(Surv(tstart, time, status) ~ 
             karno*strata(timegroup) + age + trt)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_coxph}
\keyword{coxph}
