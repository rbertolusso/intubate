\name{ntbt_cloud}
\alias{ntbt_cloud}
\title{Interface to cloud for data science pipelines.}
\description{
  \code{ntbt_cloud} is an interface to \code{lattice::\link{cloud}}
  (3d Scatter Plot) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cloud(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{cloud}}.}
}
\details{
  ntbt_cloud reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{cloud}.
}
\value{
  Object returned by \code{\link{cloud}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{cloud}}
}
\examples{
library(lattice)

## Original function to interface
cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## The interface reverses the order of data and formula
ntbt_cloud(data = iris, Sepal.Length ~ Petal.Length * Petal.Width | Species,
           screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species,
             screen = list(x = -90, y = 70), distance = .4, zoom = .6)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_cloud}
\keyword{cloud}
