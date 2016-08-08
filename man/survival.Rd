\name{ntbt_cch}
\alias{ntbt_cch}
\alias{ntbt_coxph}
\alias{ntbt_pyears}
\alias{ntbt_survConcordance}
\title{Interfaces to survival package for data science pipelines.}
\description{
  Interface to \code{survival} functions that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cch(data, ...)
ntbt_coxph(data, ...)
ntbt_pyears(data, ...)
ntbt_survConcordance(data, ...)
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
\dontrun{
library(magrittr)
library(survival)

## cch
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
ccoh.data \%>\%
  ntbt_cch(Surv(edrel, rel) ~ stage + histol + age,
    subcoh = ~subcohort, id=~seqno, cohort.size=4028)
    
## coxph    
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
veteran \%>\%
  ntbt_survSplit(Surv(time, status) ~.,
                 cut = c(60, 120), episode = "timegroup") \%>\%
  ntbt_coxph(Surv(tstart, time, status) ~ 
             karno*strata(timegroup) + age + trt)

## pyears
hearta <- by(heart, heart$id,  
             function(x)x[x$stop == max(x$stop),]) 
hearta <- do.call("rbind", hearta) 

## Original function to interface
pyears(Surv(stop/365.25, event) ~
         cut(age + 48, c(0,50,60,70,100)) + surgery,
       data = hearta, scale = 1)

## The interface reverses the order of data and formula
ntbt_pyears(data = hearta,
            Surv(stop/365.25, event) ~
              cut(age + 48, c(0,50,60,70,100)) + surgery,
            scale = 1)

## so it can be used easily in a pipeline.
hearta \%>\%
  ntbt_pyears(Surv(stop/365.25, event) ~
                cut(age + 48, c(0,50,60,70,100)) + surgery,
              scale = 1)

## survConcordance
## Original function to interface
survConcordance(Surv(time, status) ~ age, data=lung)

## The interface reverses the order of data and formula
ntbt_survConcordance(data=lung, Surv(time, status) ~ age)

## so it can be used easily in a pipeline.
lung \%>\%
  ntbt_survConcordance(Surv(time, status) ~ age)

}
}
\keyword{intubate}
\keyword{magrittr}
