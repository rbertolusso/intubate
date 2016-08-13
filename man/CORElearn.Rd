\name{CORElearn}
\alias{ntbt_attrEval}
\alias{ntbt_CoreModel}
\alias{ntbt_discretize}
\alias{ntbt_ordEval}
\title{Interfaces for CORElearn package for data science pipelines.}
\description{
  Interfaces to \code{CORElearn} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_attrEval(data, ...)
ntbt_CoreModel(data, ...)
ntbt_discretize(data, ...)
ntbt_ordEval(data, ...)
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
library(CORElearn)

## ntbt_attrEval: Attribute evaluation
## Original function to interface
attrEval(Species ~ ., iris, estimator = "ReliefFexpRank", ReliefIterations = 30)

## The interface puts data as first parameter
ntbt_attrEval(iris, Species ~ ., estimator = "ReliefFexpRank", ReliefIterations = 30)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_attrEval(Species ~ ., estimator = "ReliefFexpRank", ReliefIterations = 30)

## ntbt_CoreModel: Build a classification or regression model
trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]

## Original function to interface
CoreModel(Species ~ ., iris[trainIdxs,], model = "rf",
          selectionEstimator = "MDL", minNodeWeightRF = 5,
          rfNoTrees = 100, maxThreads = 1)

## The interface puts data as first parameter
ntbt_CoreModel(iris[trainIdxs,], Species ~ ., model = "rf",
               selectionEstimator = "MDL", minNodeWeightRF = 5,
               rfNoTrees = 100, maxThreads = 1)

## so it can be used easily in a pipeline.
iris[trainIdxs,] \%>\%
  ntbt_CoreModel(Species ~ ., model = "rf",
                 selectionEstimator = "MDL", minNodeWeightRF = 5,
                 rfNoTrees = 100, maxThreads = 1)

## ntbt_discretize: Discretization of numeric attributes
## Original function to interface
discretize(Species ~ ., iris, method = "greedy", estimator = "ReliefFexpRank")

## The interface puts data as first parameter
ntbt_discretize(iris, Species ~ ., method = "greedy", estimator = "ReliefFexpRank")

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_discretize(Species ~ ., method = "greedy", estimator = "ReliefFexpRank")

## ntbt_ordEval: Evaluation of ordered attributes
dat <- ordDataGen(200)

## Original function to interface
ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=100)

## The interface puts data as first parameter
ntbt_ordEval(dat, class ~ ., ordEvalNoRandomNormalizers=100)

## so it can be used easily in a pipeline.
dat \%>\%
  ntbt_ordEval(class ~ ., ordEvalNoRandomNormalizers=100)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{CORElearn}
\keyword{attrEval}
\keyword{CoreModel}
\keyword{discretize}
\keyword{ordEval}
