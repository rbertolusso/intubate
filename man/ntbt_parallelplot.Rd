\name{ntbt_parallelplot}
\alias{ntbt_parallelplot}
\title{Interface to parallelplot for data science pipelines.}
\description{
  \code{ntbt_parallelplot} is an interface to \code{lattice::\link{parallelplot}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_parallelplot(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{parallelplot}}.}
}
\details{
  ntbt_parallelplot reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{parallelplot}.
}
\value{
  Object returned by \code{\link{parallelplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{parallelplot}}
}
\examples{
library(lattice)

## Original function to interface
parallelplot(~iris[1:4], iris, groups = Species,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

## The interface reverses the order of data and formula
ntbt_parallelplot(iris, ~iris[1:4], groups = Species,
                  horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_parallelplot(~iris[1:4], groups = Species,
                    horizontal.axis = FALSE, scales = list(x = list(rot = 90)))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_parallelplot}
\keyword{parallelplot}
