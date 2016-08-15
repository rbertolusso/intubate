\name{adabag}
\alias{ntbt_autoprune}
%\alias{ntbt_bagging} Already defined in ipred
\alias{ntbt_bagging.cv}
\alias{ntbt_boosting}
\alias{ntbt_boosting.cv}
\title{Interfaces for adabag package for data science pipelines.}
\description{
  Interfaces to \code{adabag} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_autoprune(data, ...)
# ntbt_bagging(data, ...)  ## Already defined in ipred
ntbt_bagging.cv(data, ...)
ntbt_boosting(data, ...)
ntbt_boosting.cv(data, ...)
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
library(adabag)


## ntbt_autoprune: Builds automatically a pruned tree of class rpart
## Original function to interface
autoprune(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_autoprune(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_autoprune(Species ~ .)

## ntbt_bagging: Applies the Bagging algorithm to a data set
library(rpart)
data(iris)

## Original function to interface
bagging(Species ~ ., data = iris, mfinal = 10)

## The interface puts data as first parameter
ntbt_bagging(iris, Species ~ ., mfinal = 10)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_bagging(Species ~ ., mfinal = 10)



## Original function to interface
iris.baggingcv <- bagging.cv(Species ~ ., v = 2, data = iris, mfinal = 10,
                             control = rpart.control(cp = 0.01))
iris.baggingcv[-1]

## The interface puts data as first parameter
iris.baggingcv <- ntbt_bagging.cv(iris, Species ~ ., v = 2, mfinal = 10,
                                  control = rpart.control(cp = 0.01))
iris.baggingcv[-1]

## so it can be used easily in a pipeline.
iris.baggingcv <- iris \%>\%
  ntbt_bagging.cv(Species ~ ., v = 2, mfinal = 10,
                  control = rpart.control(cp = 0.01))
iris.baggingcv[-1]


## ntbt_boosting: Applies the AdaBoost.M1 and SAMME algorithms to a data set
## Original function to interface
boosting(Species ~ ., data = iris, boos = TRUE, mfinal = 5)

## The interface puts data as first parameter
ntbt_boosting(iris, Species ~ ., boos = TRUE, mfinal = 5)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_boosting(Species ~ ., boos = TRUE, mfinal = 5)


## ntbt_boosting.cv: Runs v-fold cross validation with AdaBoost.M1 or SAMME
## Original function to interface
iris.boostcv <- boosting.cv(Species ~ ., v = 2, data = iris, mfinal = 10, 
                            control = rpart.control(cp = 0.01))
iris.boostcv[-1]

## The interface puts data as first parameter
iris.boostcv <- ntbt_boosting.cv(iris, Species ~ ., v = 2, mfinal = 10, 
                                 control = rpart.control(cp = 0.01))
iris.boostcv[-1]

## so it can be used easily in a pipeline.
iris.boostcv <- iris \%>\%
  ntbt_boosting.cv(Species ~ ., v = 2, mfinal = 10, 
                   control = rpart.control(cp = 0.01))
iris.boostcv[-1]
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{adabag}
\keyword{autoprune}
\keyword{bagging}
\keyword{bagging.cv}
\keyword{boosting}
\keyword{boosting.cv}
