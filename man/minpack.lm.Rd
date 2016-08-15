\name{minpack.lm}
\alias{ntbt_nlsLM}
\title{Interfaces for minpack.lm package for data science pipelines.}
\description{
  Interfaces to \code{minpack.lm} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_nlsLM(data, ...)
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
library(minpack.lm)

## ntbt_nlsLM: Standard 'nls' framework that uses 'nls.lm' for fitting
DNase1 <- subset(DNase, Run == 1)

## Original function to interface
nlsLM(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
      data = DNase1,
      start = list(Asym = 3, xmid = 0, scal = 1))

## The interface puts data as first parameter
ntbt_nlsLM(DNase1, density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
           start = list(Asym = 3, xmid = 0, scal = 1))

## so it can be used easily in a pipeline.
DNase1 \%>\%
  ntbt_nlsLM(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
             start = list(Asym = 3, xmid = 0, scal = 1))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{minpack.lm}
\keyword{nlsLM}
