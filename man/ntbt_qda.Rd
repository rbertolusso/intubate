\name{ntbt_qda}
\alias{ntbt_qda}
\title{Interface to qda for data science pipelines.}
\description{
  \code{ntbt_qda} is an interface to \code{MASS::\link{qda}}
  (Quadratic Discriminant Analysis) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_qda(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{qda}}.}
}
\details{
  ntbt_qda reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{qda}}.
}
\value{
  Object returned by \code{\link{qda}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{qda}}
}
\examples{
set.seed(123) ## make reproducible
tr <- sample(1:50, 25)

iris3df <- data.frame(cl = factor(c(rep("s",25), rep("c",25), rep("v",25))),
                      train = rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3]))

library(MASS)

## Original function to interface
qda(cl ~ ., iris3df)

## The interface reverses the order of data and formula
ntbt_qda(iris3df, cl ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
iris3df \%>\%
  ntbt_qda(cl ~ .)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_qda}
\keyword{qda}
