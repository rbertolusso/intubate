\name{iRegression}
\alias{ntbt_bivar}
\alias{ntbt_ccrm}
\alias{ntbt_cm}
\alias{ntbt_crm}
\alias{ntbt_MinMax}
\title{Interfaces for iRegression package for data science pipelines.}
\description{
  Interfaces to \code{iRegression} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bivar(data, ...)
ntbt_ccrm(data, ...)
ntbt_cm(data, ...)
ntbt_crm(data, ...)
ntbt_MinMax(data, ...)
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
library(iRegression)

## bivar
data("soccer.bivar", package = "iRegression")

## Original function to interface
bivar(yMin ~ t1Min + t2Min, "identity", yMax ~ t1Max + t2Max, "identity", data = soccer.bivar)

## The interface puts data as first parameter
ntbt_bivar(soccer.bivar, yMin ~ t1Min + t2Min, "identity", yMax ~ t1Max + t2Max, "identity")

## so it can be used easily in a pipeline.
soccer.bivar \%>\%
  ntbt_bivar(yMin ~ t1Min + t2Min, "identity", yMax ~ t1Max + t2Max, "identity")

## ntbt_ccrm
data("Cardiological.CR", package = "iRegression")
## Original function to interface
ccrm(PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR, data = Cardiological.CR)

## The interface puts data as first parameter
ntbt_ccrm(Cardiological.CR, PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR)

## so it can be used easily in a pipeline.
Cardiological.CR \%>\%
  ntbt_ccrm(PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR)

## ntbt_cm
data("Cardiological.MinMax", package = "iRegression") ## see Billard and Diday (2000)
## Original function to interface
cm(PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax, data = Cardiological.MinMax)

## The interface puts data as first parameter
ntbt_cm(Cardiological.MinMax, PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax)

## so it can be used easily in a pipeline.
Cardiological.MinMax \%>\%
  ntbt_cm(PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax)

## ntbt_crm
data("Cardiological.CR", package = "iRegression")
## Original function to interface
crm(PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR, data = Cardiological.CR)

## The interface puts data as first parameter
ntbt_crm(Cardiological.CR, PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR)

## so it can be used easily in a pipeline.
Cardiological.CR \%>\%
  ntbt_crm(PulseC ~ SystC + DiastC, PulseR ~ SystR + DiastR)

## ntbt_MinMax
data("Cardiological.MinMax", package = "iRegression") ## see Billard, L. and Diday, E. (2000)
## Original function to interface
MinMax(PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax, data = Cardiological.MinMax)

## The interface puts data as first parameter
ntbt_MinMax(Cardiological.MinMax, PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax)

## so it can be used easily in a pipeline.
Cardiological.MinMax \%>\%
  ntbt_MinMax(PulseMin ~ SystMin + DiastMin, PulseMax ~ SystMax + DiastMax)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{iRegression}
\keyword{bivar}
\keyword{ccrm}
\keyword{cm}
\keyword{crm}
\keyword{MinMax}
