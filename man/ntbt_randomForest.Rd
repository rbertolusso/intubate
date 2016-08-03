\name{ntbt_randomForest}
\alias{ntbt_randomForest}
\title{Interface to randomForest for data science pipelines.}
\description{
  \code{ntbt_randomForest} is an interface to \code{randomForest::\link{randomForest}}
  (Classification and Regression with Random Forest) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_randomForest(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{randomForest}}.}
}
\details{
  ntbt_randomForest calls \code{randomForest}.
}
\value{
  Object returned by randomForest function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{randomForest}}
}
\examples{
library(randomForest)

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
library(magrittr)
set.seed(71)
iris \%>\%
  ntbt_randomForest(Species ~ ., importance = TRUE,
                    proximity = TRUE) \%>\%
  plot()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_randomForest}
\keyword{randomForest}
