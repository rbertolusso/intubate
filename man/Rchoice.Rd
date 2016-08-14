\name{Rchoice}
\alias{ntbt_Rchoice}
\title{Interfaces for Rchoice package for data science pipelines.}
\description{
  Interfaces to \code{Rchoice} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_Rchoice(data, ...)
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
library(Rchoice)

## ntbt_Rchoice: Bias reduction in Binomial-response GLMs
data("Workmroz")

## Original function to interface
Rchoice(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
        data = Workmroz, family = binomial('probit'))
## The interface puts data as first parameter
ntbt_Rchoice(Workmroz, lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
             family = binomial('probit'))

## so it can be used easily in a pipeline.
Workmroz \%>\%
  ntbt_Rchoice(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
               family = binomial('probit'))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{Rchoice}
