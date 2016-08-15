\name{sem}
\alias{ntbt_rawMoments}
\alias{ntbt_sem}
\alias{ntbt_tsls}
\title{Interfaces for sem package for data science pipelines.}
\description{
  Interfaces to \code{sem} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_rawMoments(data, ...)
ntbt_sem(data, ...)
ntbt_tsls(data, ...)
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
library(sem)


## ntbt_rawMoments: Compute Raw Moments Matrix
## Original function to interface
rawMoments(~ Q + P + D + F + A, data = Kmenta)

## The interface puts data as first parameter
ntbt_rawMoments(Kmenta, ~ Q + P + D + F + A)

## so it can be used easily in a pipeline.
Kmenta \%>\%
  ntbt_rawMoments(~ Q + P + D + F + A)


## ntbt_sem: General Structural Equation Models
## NOTE: this example is NOT using the formula interface.
##       It is creating a list with the variables.
R.DHP <- readMoments(diag=FALSE, names=c("ROccAsp", "REdAsp", "FOccAsp", 
                "FEdAsp", "RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"),
                text="
    .6247     
    .3269  .3669       
    .4216  .3275  .6404
    .2137  .2742  .1124  .0839
    .4105  .4043  .2903  .2598  .1839
    .3240  .4047  .3054  .2786  .0489  .2220
    .2930  .2407  .4105  .3607  .0186  .1861  .2707
    .2995  .2863  .5191  .5007  .0782  .3355  .2302  .2950
    .0760  .0702  .2784  .1988  .1147  .1021  .0931 -.0438  .2087
")

model.dhp.1 <- specifyEquations(covs="RGenAsp, FGenAsp", text="
RGenAsp = gam11*RParAsp + gam12*RIQ + gam13*RSES + gam14*FSES + beta12*FGenAsp
FGenAsp = gam23*RSES + gam24*FSES + gam25*FIQ + gam26*FParAsp + beta21*RGenAsp
ROccAsp = 1*RGenAsp
REdAsp = lam21(1)*RGenAsp  # to illustrate setting start values
FOccAsp = 1*FGenAsp
FEdAsp = lam42(1)*FGenAsp
")

dta <- list(R.DHP = R.DHP, model.dhp.1 = model.dhp.1)
rm(R.DHP, model.dhp.1)

## Original function to interface
attach(dta)
sem.dhp.1 <- ntbt_sem(model.dhp.1, R.DHP, 329,
                      fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp'))
summary(sem.dhp.1)
detach()

## The interface puts data as first parameter
sem.dhp.1 <- ntbt_sem(dta, model.dhp.1, R.DHP, 329,
                      fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp'))
summary(sem.dhp.1)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_sem(model.dhp.1, R.DHP, 329,
           fixed.x=c('RParAsp', 'RIQ', 'RSES', 'FSES', 'FIQ', 'FParAsp')) %>%
  summary()


## ntbt_tsls: Two-Stage Least Squares
## Original function to interface
tsls(Q ~ P + D, ~ D + F + A, data = Kmenta)

## The interface puts data as first parameter
ntbt_tsls(Kmenta, Q ~ P + D, ~ D + F + A)

## so it can be used easily in a pipeline.
Kmenta \%>\%
  ntbt_tsls(Q ~ P + D, ~ D + F + A)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{sem}
\keyword{rawMoments}
\keyword{sem}
\keyword{tsls}
