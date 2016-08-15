\name{aod}
\alias{ntbt_betabin}
\alias{ntbt_donner}
\alias{ntbt_negbin}
\alias{ntbt_quasibin}
\alias{ntbt_quasipois}
\alias{ntbt_raoscott}
\alias{ntbt_splitbin}
\title{Interfaces for aod package for data science pipelines.}
\description{
  Interfaces to \code{aod} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_betabin(data, ...)
ntbt_donner(data, ...)
ntbt_negbin(data, ...)
ntbt_quasibin(data, ...)
ntbt_quasipois(data, ...)
ntbt_raoscott(data, ...)
ntbt_splitbin(data, ...)
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
library(aod)


## ntbt_betabin: beta-binomial generalized linear model accounting
##               for overdispersion in clustered binomial data (n, y)
data(orob2)
## Original function to interface
betabin(cbind(y, n - y) ~ seed, ~ 1, data = orob2)

## The interface puts data as first parameter
ntbt_betabin(orob2, cbind(y, n - y) ~ seed, ~ 1)

## so it can be used easily in a pipeline.
orob2 \%>\%
  ntbt_betabin(cbind(y, n - y) ~ seed, ~ 1)


## ntbt_donner: Test of Proportion Homogeneity using Donner's Adjustment
data(rats)

## Original function to interface
donner(formula = cbind(y, n - y) ~ group, data = rats)

## The interface puts data as first parameter
ntbt_donner(rats, formula = cbind(y, n - y) ~ group)

## so it can be used easily in a pipeline.
rats \%>\%
  ntbt_donner(formula = cbind(y, n - y) ~ group)


## ntbt_negbin: negative-binomial log linear model accounting
##              for overdispersion in counts y
data(salmonella)
## Original function to interface
negbin(y ~ log(dose + 10) + dose, ~ 1, salmonella)

## The interface puts data as first parameter
ntbt_negbin(salmonella, y ~ log(dose + 10) + dose, ~ 1)

## so it can be used easily in a pipeline.
salmonella \%>\%
  ntbt_negbin(y ~ log(dose + 10) + dose, ~ 1)


## ntbt_quasibin: Quasi-Likelihood Model for Proportions
data(orob2) 
## Original function to interface
quasibin(cbind(y, n - y) ~ seed * root, data = orob2, phi = 0) 

## The interface puts data as first parameter
ntbt_quasibin(orob2, cbind(y, n - y) ~ seed * root, phi = 0)

## so it can be used easily in a pipeline.
orob2 \%>\%
  ntbt_quasibin(cbind(y, n - y) ~ seed * root, phi = 0)


## ntbt_quasipois: Quasi-Likelihood Model for Counts
data(salmonella)

## Original function to interface
quasipois(y ~ log(dose + 10) + dose, data = salmonella)

## The interface puts data as first parameter
ntbt_quasipois(salmonella, y ~ log(dose + 10) + dose)

## so it can be used easily in a pipeline.
salmonella \%>\%
  ntbt_quasipois(y ~ log(dose + 10) + dose)


## ntbt_raoscott: Test of Proportion Homogeneity using Rao and Scott's Adjustment
data(rats)

## Original function to interface
raoscott(cbind(y, n - y) ~ group, data = rats)

## The interface puts data as first parameter
ntbt_raoscott(rats, cbind(y, n - y) ~ group)

## so it can be used easily in a pipeline.
rats \%>\%
  ntbt_raoscott(cbind(y, n - y) ~ group)


## ntbt_splitbin: Split Grouped Data Into Individual Data
mydata <- data.frame(
    success = c(0, 1, 0, 1),
    f1 = c("A", "A", "B", "B"),
    f2 = c("C", "D", "C", "D"),
    n = c(4, 2, 1, 3)
    )
## Original function to interface
splitbin(formula = n ~ f1 + f2 + success, data = mydata)

## The interface puts data as first parameter
ntbt_splitbin(mydata, formula = n ~ f1 + f2 + success)

## so it can be used easily in a pipeline.
mydata \%>\%
  ntbt_splitbin(formula = n ~ f1 + f2 + success)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{aod}
\keyword{betabin}
\keyword{donner}
\keyword{negbin}
\keyword{quasibin}
\keyword{quasipois}
\keyword{raoscott}
\keyword{splitbin}
