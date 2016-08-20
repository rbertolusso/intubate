\name{frontier}
\alias{ntbt_sfa}
\title{Interfaces for frontier package for data science pipelines.}
\description{
  Interfaces to \code{frontier} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_sfa(data, ...)
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
library(frontier)


## ntbt_sfa: Stochastic Frontier Analysis
data(front41Data)

## Original function to interface
sfa(log(output) ~ log(capital) + log(labour), data = front41Data)

## The interface puts data as first parameter
ntbt_sfa(front41Data, log(output) ~ log(capital) + log(labour))

## so it can be used easily in a pipeline.
front41Data \%>\%
  ntbt_sfa(log(output) ~ log(capital) + log(labour))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{frontier}
\keyword{sfa}
