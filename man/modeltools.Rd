\name{modeltools}
\alias{ntbt_ModelEnvFormula}
\alias{ntbt_ParseFormula}
\title{Interfaces for modeltools package for data science pipelines.}
\description{
  Interfaces to \code{modeltools} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_ModelEnvFormula(data, ...)
ntbt_ParseFormula(data, ...)
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
library(magrittr)
library(modeltools)

data("iris")

## Original function to interface
ModelEnvFormula(Species + Petal.Width ~ . -1, data = iris,
                subset = sample(1:150, 10))
ParseFormula(Species + Petal.Width ~ . -1, data = iris)

## The interface puts data as first parameter
ntbt_ModelEnvFormula(iris, Species + Petal.Width ~ . -1, subset = sample(1:150, 10))
ntbt_ParseFormula(iris, Species + Petal.Width ~ . -1)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_ModelEnvFormula(Species + Petal.Width ~ . -1, subset = sample(1:150, 10))

iris \%>\%
  ntbt_ParseFormula(Species + Petal.Width ~ . -1)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{modeltools}
\keyword{ModelEnvFormula}
\keyword{ParseFormula}
