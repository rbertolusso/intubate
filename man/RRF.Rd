\name{RRF}
\alias{ntbt_RRF}
\alias{ntbt_rrfImpute}
\title{Interfaces for RRF package for data science pipelines.}
\description{
  Interfaces to \code{RRF} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_RRF(data, ...)
ntbt_rrfImpute(data, ...)
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
library(RRF)


data(iris)
set.seed(111)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))

## Original function to interface
RRF(Species ~ ., data=iris[ind == 1,])

## The interface puts data as first parameter
ntbt_RRF(iris[ind == 1,], Species ~ .)

## so it can be used easily in a pipeline.
iris[ind == 1,] \%>\%
  ntbt_RRF(Species ~ .)


## ntbt_rrfImpute: Missing Value Imputations by RRF
data(iris)
iris.na <- iris
set.seed(111)
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA

## Original function to interface
set.seed(222)
rrfImpute(Species ~ ., iris.na)

## The interface puts data as first parameter
set.seed(222)
ntbt_rrfImpute(iris.na, Species ~ .)

## so it can be used easily in a pipeline.
set.seed(222)
iris.na \%>\%
  ntbt_rrfImpute(Species ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{RRF}
\keyword{rrfImpute}
