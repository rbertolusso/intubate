\name{ntbt_kruskal.test}
\alias{ntbt_kruskal.test}
\title{Interface to kruskal.test for data science pipelines.}
\description{
  \code{ntbt_kruskal.test} is an interface to \code{stats::\link{kruskal.test}}
  (Kruskal-Wallis Rank Sum Test) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_kruskal.test(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{kruskal.test}}.}
}
\details{
  ntbt_kruskal.test reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{kruskal.test}.
}
\value{
  Object returned by kruskal.test function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{kruskal.test}}
}
\examples{
## Original function to interface
kruskal.test(Ozone ~ Month, airquality)

## The interface reverses the order of data and formula
ntbt_kruskal.test(airquality, Ozone ~ Month)

## so it can be used easily in a pipeline.
library(magrittr)
airquality \%>\%
  ntbt_kruskal.test(Ozone ~ Month)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_kruskal.test}
\keyword{kruskal.test}
