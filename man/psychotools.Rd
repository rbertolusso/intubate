\name{psychotools}
\alias{ntbt_anchor}
\alias{ntbt_anchortest}
\title{Interfaces for psychotools package for data science pipelines.}
\description{
  Interfaces to \code{psychotools} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_anchor(data, ...)
ntbt_anchortest(data, ...)
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
library(psychotools)


## ntbt_anchor: Anchor Methods for the Detection of Uniform DIF the Rasch Model
data("VerbalAggression", package = "psychotools")

## Original function to interface
anchor(resp2[, 1:12] ~ gender, data = VerbalAggression,
       class = "forward", select = "MTT", range = c(0.05, 1))

## The interface puts data as first parameter
ntbt_anchor(VerbalAggression, resp2[, 1:12] ~ gender, 
            class = "forward", select = "MTT", range = c(0.05, 1))

## so it can be used easily in a pipeline.
VerbalAggression \%>\%
  ntbt_anchor(resp2[, 1:12] ~ gender, 
              class = "forward", select = "MTT", range = c(0.05, 1))


## ntbt_anchortest: Anchor methods for the detection of uniform DIF the Rasch model

## Original function to interface
anchortest(resp2[, 1:12] ~ gender, data = VerbalAggression,
           class = "forward", select = "MTT", range = c(0.05, 1))

## The interface puts data as first parameter
ntbt_anchortest(VerbalAggression, resp2[, 1:12] ~ gender,
                class = "forward", select = "MTT", range = c(0.05, 1))

## so it can be used easily in a pipeline.
VerbalAggression \%>\%
  ntbt_anchortest(resp2[, 1:12] ~ gender,
                  class = "forward", select = "MTT", range = c(0.05, 1))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{psychotools}
\keyword{anchor}
\keyword{anchortest}
