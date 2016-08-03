\name{ntbt_cch}
\alias{ntbt_cch}
\title{Interface to cch for data science pipelines.}
\description{
  \code{ntbt_cch} is an interface to \code{survival::\link{cch}}
  (Compute a Survival Curve for Censored Data) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cch(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{cch}}.}
}
\details{
  ntbt_cch calls \code{\link{cch}}.
}
\value{
  Object returned by \code{\link{cch}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{cch}}
}
\examples{
library(survival)

subcoh <- nwtco$in.subcohort
selccoh <- with(nwtco, rel==1|subcoh==1)
ccoh.data <- nwtco[selccoh,]
ccoh.data$subcohort <- subcoh[selccoh]
ccoh.data$histol <- factor(ccoh.data$histol,labels=c("FH","UH"))
ccoh.data$stage <- factor(ccoh.data$stage,labels=c("I","II","III","IV"))
ccoh.data$age <- ccoh.data$age/12 # Age in years

## Original function to interface
cch(Surv(edrel, rel) ~ stage + histol + age, data = ccoh.data,
    subcoh = ~subcohort, id=~seqno, cohort.size=4028)

## The interface reverses the order of data and formula
ntbt_cch(data = ccoh.data, Surv(edrel, rel) ~ stage + histol + age,
    subcoh = ~subcohort, id=~seqno, cohort.size=4028)

## so it can be used easily in a pipeline.
library(magrittr)
ccoh.data \%>\%
  ntbt_cch(Surv(edrel, rel) ~ stage + histol + age,
    subcoh = ~subcohort, id=~seqno, cohort.size=4028)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_cch}
\keyword{cch}
