\name{ntbt_model.matrix}
\alias{ntbt_model.matrix}
\title{Interface to model.matrix for data science pipelines.}
\description{
  \code{ntbt_model.matrix} is an interface to \code{stats::\link{model.matrix}}
  (Construct Design Matrices) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_model.matrix(data, object, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{object}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{model.matrix}}.}
}
\details{
  ntbt_model.matrix reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{model.matrix}.
}
\value{
  Object returned by \code{\link{model.matrix}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{model.matrix}}
}
\examples{
dd <- data.frame(a = gl(3, 4),
                 b = gl(4, 1, 12)) # balanced 2-way

## Original function to interface
model.matrix(~ a + b, dd)

## The interface reverses the order of data and formula
ntbt_model.matrix(dd, ~ a + b)

## so it can be used easily in a pipeline.
library(magrittr)
dd \%>\%
  ntbt_model.matrix(~ a + b)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_model.matrix}
\keyword{model.matrix}
