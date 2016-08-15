\name{ordinal}
\alias{ntbt_clm}
\alias{ntbt_clm2}
\alias{ntbt_clmm}
\title{Interfaces for ordinal package for data science pipelines.}
\description{
  Interfaces to \code{ordinal} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_clm(data, ...)
ntbt_clm2(data, ...)
ntbt_clmm(data, ...)
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
library(ordinal)


## ntbt_clm: Cumulative Link Models
## ntbt_clm2:
## Original function to interface
clm(rating ~ temp * contact, data = wine)
clm2(rating ~ temp * contact, data = wine)

## The interface puts data as first parameter
ntbt_clm(wine, rating ~ temp * contact)
ntbt_clm2(wine, rating ~ temp * contact)

## so it can be used easily in a pipeline.
wine \%>\%
  ntbt_clm(rating ~ temp * contact)
wine \%>\%
  ntbt_clm2(rating ~ temp * contact)


## ntbt_clmm: Cumulative Link Mixed Models
## Original function to interface
clmm(SURENESS ~ PROD + (1|RESP) + (1|RESP:PROD), data = soup,
     link = "probit", threshold = "equidistant")

## The interface puts data as first parameter
ntbt_clmm(soup, SURENESS ~ PROD + (1|RESP) + (1|RESP:PROD),
          link = "probit", threshold = "equidistant")

## so it can be used easily in a pipeline.
soup \%>\%
  ntbt_clmm(SURENESS ~ PROD + (1|RESP) + (1|RESP:PROD),
            link = "probit", threshold = "equidistant")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ordinal}
\keyword{clm}
\keyword{clm2}
\keyword{clmm}
