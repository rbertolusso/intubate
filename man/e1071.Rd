\name{e1071}
\alias{ntbt_svm}
\title{Interfaces for e1071 package for data science pipelines.}
\description{
  Interfaces to \code{e1071} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_svm(data, ...)
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
library(e1071)

## Original function to interface
model <- svm(Species ~ ., iris)
summary(model)

## The interface reverses the order of data and formula
model <- ntbt_svm(iris, Species ~ .)
summary(model)

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_svm(Species ~ .) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{e1071}
\keyword{svm}
