\name{ntbt_xyplot}
\alias{ntbt_xyplot}
\title{Interface to xyplot for data science pipelines.}
\description{
  \code{ntbt_xyplot} is an interface to \code{lattice::\link{xyplot}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_xyplot(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{xyplot}}.}
}
\details{
  ntbt_xyplot reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{xyplot}.
}
\value{
  Object returned by \code{\link{xyplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{xyplot}}
}
\examples{
library(lattice)

## Original function to interface
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## The interface reverses the order of data and formula
ntbt_xyplot(data = iris, 
            Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
            scales = "free", layout = c(2, 2),
            auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
              scales = "free", layout = c(2, 2),
              auto.key = list(x = .6, y = .7, corner = c(0, 0)))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_xyplot}
\keyword{xyplot}
