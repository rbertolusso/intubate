\name{ntbt_finegray}
\alias{ntbt_finegray}
\title{Interface to finegray for data science pipelines.}
\description{
  \code{ntbt_finegray} is an interface to \code{survival::\link{finegray}}
  (Create data for a Fine-Gray model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_finegray(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{finegray}}.}
}
\details{
  ntbt_finegray calls \code{\link{finegray}}.
}
\value{
  Object returned by \code{\link{finegray}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{finegray}}
}
\examples{
library(survival)

# Treat time to death and plasma cell malignancy as competing risks
etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
# FG model for PCM

## Original function to interface
pdata <- finegray(Surv(etime, event) ~ ., data = mgus2)
coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex, data = pdata,
      weight = fgwt)

## The interface reverses the order of data and formula
pdata <- ntbt_finegray(data = mgus2, Surv(etime, event) ~ .)
ntbt_coxph(data = pdata, Surv(fgstart, fgstop, fgstatus) ~ age + sex,
           weight = fgwt)

## so it can be used easily in a pipeline.
library(magrittr)
mgus2 \%>\%
  ntbt_finegray(Surv(etime, event) ~ .) \%>\%
  ntbt_coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex,
             weight = fgwt)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_finegray}
\keyword{finegray}
