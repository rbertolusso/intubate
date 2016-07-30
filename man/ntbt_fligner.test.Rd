\name{ntbt_fligner.test}
\alias{ntbt_fligner.test}
\title{Interface to fligner.test for data science pipelines.}
\description{
  \code{ntbt_fligner.test} is an interface to \code{stats::\link{fligner.test}}
  (Fligner-Killeen Test of Homogeneity of Variances) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_fligner.test(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{fligner.test}}.}
}
\details{
  ntbt_fligner.test reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{fligner.test}.
}
\value{
  Object returned by \code{\link{fligner.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{fligner.test}}
}
\examples{
## Original function to interface
fligner.test(count ~ spray, data = InsectSprays)

## The interface reverses the order of data and formula
ntbt_fligner.test(data = InsectSprays, count ~ spray)

## so it can be used easily in a pipeline.
library(magrittr)
InsectSprays \%>\%
  ntbt_fligner.test(count ~ spray)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_fligner.test}
\keyword{fligner.test}
