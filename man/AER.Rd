\name{AER}
\alias{ntbt_ivreg}
\alias{ntbt_tobit}
\title{Interfaces for AER package for data science pipelines.}
\description{
  Interfaces to \code{AER} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_ivreg(data, ...)
ntbt_tobit(data, ...)
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
library(AER)


## ntbt_ivreg: Instrumental-Variable Regression
data("CigarettesSW", package = "AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

## Original function to interface
ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
      data = CigarettesSW, subset = year == "1995")

## The interface puts data as first parameter
ntbt_ivreg(CigarettesSW, log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
           subset = year == "1995")

## so it can be used easily in a pipeline.
CigarettesSW \%>\%
  ntbt_ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
             subset = year == "1995")


## ntbt_tobit: Tobit Regression
data("Affairs")

## Original function to interface
tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
      data = Affairs)

## The interface puts data as first parameter
ntbt_tobit(Affairs, affairs ~ age + yearsmarried + religiousness + occupation + rating)

## so it can be used easily in a pipeline.
Affairs \%>\%
  ntbt_tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{AER}
\keyword{ivreg}
\keyword{tobit}
