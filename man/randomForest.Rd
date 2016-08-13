\name{randomForest}
\alias{ntbt_randomForest}
\title{Interfaces for randomForest package for data science pipelines.}
\description{
  Interfaces to \code{randomForest} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_randomForest(data, ...)
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
library(randomForest)

## randomForest
## Original function to interface
set.seed(71)
iris.rf <- randomForest(Species ~ ., data = iris,
                        importance = TRUE, proximity = TRUE)
print(iris.rf)
plot(iris.rf)

## The interface reverses the order of data and formula
set.seed(71)
iris.rf <- ntbt_randomForest(iris, Species ~ .,
                             importance = TRUE, proximity = TRUE)
print(iris.rf)
plot(iris.rf)

## so it can be used easily in a pipeline.
set.seed(71)
iris \%>\%
  ntbt_randomForest(Species ~ ., importance = TRUE,
                    proximity = TRUE) \%>\%
  plot()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{randomForest}
