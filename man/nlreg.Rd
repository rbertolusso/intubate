\name{nlreg}
\alias{ntbt_nlreg}
\title{Interfaces for nlreg package for data science pipelines.}
\description{
  Interfaces to \code{nlreg} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_nlreg(data, ...)
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
library(nlreg)


## ntbt_nlreg: Fit a Nonlinear Heteroscedastic Model via Maximum Likelihood
library(boot)
data(calcium)

## Original function to interface
nlreg(cal ~ b0*(1-exp(-b1*time)), weights = ~ ( 1+time^g )^2,
      start = c(b0 = 4, b1 = 0.1, g = 1), data = calcium, hoa = TRUE)

## The interface puts data as first parameter
ntbt_nlreg(calcium, cal ~ b0*(1-exp(-b1*time)), weights = ~ ( 1+time^g )^2,
           start = c(b0 = 4, b1 = 0.1, g = 1), hoa = TRUE)

## so it can be used easily in a pipeline.
calcium \%>\%
  ntbt_nlreg(cal ~ b0*(1-exp(-b1*time)), weights = ~ ( 1+time^g )^2,
             start = c(b0 = 4, b1 = 0.1, g = 1), hoa = TRUE)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{nlreg}
