\name{systemfit}
\alias{ntbt_systemfit}
\title{Interfaces for systemfit package for data science pipelines.}
\description{
  Interfaces to \code{systemfit} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_systemfit(data, ...)
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
library(systemfit)


## ntbt_systemfit: Linear Equation System Estimation
data("Kmenta")
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )


## Original function to interface
fitols <- systemfit(system, data = Kmenta)
print(fitols)

## The interface puts data as first parameter
fitols <- ntbt_systemfit(Kmenta, system, "<|F|>")
## Need "<|F|>" (at least for now) to clarify it is a formula
print(fitols)

## so it can be used easily in a pipeline.
## Need "<|F|>" (at least for now) to clarify it is a formula
Kmenta \%>\%
  ntbt_systemfit(system, "<|F|>") \%>\%
  print()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{systemfit}
