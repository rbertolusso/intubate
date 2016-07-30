\name{ntbt_svm}
\alias{ntbt_svm}
\title{Interface to svm for data science pipelines.}
\description{
  \code{ntbt_svm} is an interface to \code{e1071::\link{svm}}
  (Support Vector Machines) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_svm(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{svm}}.}
}
\details{
  ntbt_svm reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{svm}.
}
\value{
  Object returned by \code{\link{svm}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{svm}}
}
\examples{
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
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_svm}
\keyword{svm}
