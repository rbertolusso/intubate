\name{SwarmSVM}
\alias{ntbt_alphasvm}
\title{Interfaces for SwarmSVM package for data science pipelines.}
\description{
  Interfaces to \code{SwarmSVM} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_alphasvm(data, ...)
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
library(SwarmSVM)

## ntbt_alphasvm: Support Vector Machines taking initial alpha values
data(iris)
## Original function to interface
alphasvm(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_alphasvm(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_alphasvm(Species ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{SwarmSVM}
\keyword{alphasvm}
