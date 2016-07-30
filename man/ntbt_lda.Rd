\name{ntbt_lda}
\alias{ntbt_lda}
\title{Interface to lda for data science pipelines.}
\description{
  \code{ntbt_lda} is an interface to \code{MASS::\link{lda}}
  (Linear Discriminant Analysis) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lda(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lda}}.}
}
\details{
  ntbt_lda reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{lda}}.
}
\value{
  Object returned by \code{\link{lda}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lda}}
}
\examples{
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))
library(MASS)

## Original function to interface
lda(Sp ~ ., Iris)

## The interface reverses the order of data and formula
ntbt_lda(Iris, Sp ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
Iris \%>\%
  ntbt_lda(Sp ~ .)

stackloss \%>\%
  ntbt_lda(stack.loss ~ .) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lda}
\keyword{lda}
