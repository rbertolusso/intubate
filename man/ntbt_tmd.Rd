\name{ntbt_tmd}
\alias{ntbt_tmd}
\title{Interface to tmd for data science pipelines.}
\description{
  \code{ntbt_tmd} is an interface to \code{lattice::\link{tmd}}
  (Tukey Mean-Difference Plot) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_tmd(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{tmd}}.}
}
\details{
  ntbt_tmd calls \code{tmd}.
}
\value{
  Object returned by \code{\link{tmd}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{tmd}}
}
\examples{
library(lattice)

## Original function to interface
tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
    data = iris, scales = "free", layout = c(2, 2),
    auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## The interface reverses the order of data and formula
ntbt_tmd(data = iris, 
         Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         scales = "free", layout = c(2, 2),
         auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
           scales = "free", layout = c(2, 2),
           auto.key = list(x = .6, y = .7, corner = c(0, 0)))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_tmd}
\keyword{tmd}
