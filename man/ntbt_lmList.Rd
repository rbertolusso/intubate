\name{ntbt_lmList}
\alias{ntbt_lmList}
\title{Interface to lmList for data science pipelines.}
\description{
  \code{ntbt_lmList} is an interface to \code{nlme::\link{lmList}}
  (List of lm Objects with a Common Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lmList(data, object, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{object}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lmList}}.}
}
\details{
  ntbt_lmList reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{lmList}.
}
\value{
  Object returned by \code{\link{lmList}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lmList}}
}
\examples{
library(nlme)

## Original function to interface
lmList(distance ~ age | Subject, Orthodont)

## The interface reverses the order of data and formula
ntbt_lmList(Orthodont, distance ~ age | Subject)

## so it can be used easily in a pipeline.
library(magrittr)
Orthodont \%>\%
  ntbt_lmList(distance ~ age | Subject)
  
Orthodont \%>\%
  ntbt_lmList(distance ~ age | Subject) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lmList}
\keyword{lmList}
