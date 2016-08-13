\name{mda}
\alias{ntbt_fda}
\alias{ntbt_mda}
\title{Interfaces for mda package for data science pipelines.}
\description{
  Interfaces to \code{mda} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_fda(data, ...)
ntbt_mda(data, ...)
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
library(mda)

## ntbt_fda: Flexible Discriminant Analysis
data(iris)
## Original function to interface
fda(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_fda(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_fda(Species ~ .)

## ntbt_mda: Mixture Discriminant Analysis
data(iris)
## Original function to interface
mda(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_mda(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_mda(Species ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{mda}
\keyword{fda}
\keyword{mda}
