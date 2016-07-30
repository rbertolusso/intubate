\name{ntbt_lm}
\alias{ntbt_lm}
\title{Interface to lm for data science pipelines.}
\description{
  \code{ntbt_lm} is an interface to \code{stats::\link{lm}}
  (Fitting Linear Models) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lm(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lm}}.}
}
\details{
  ntbt_lm reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{lm}.
}
\value{
  Object returned by lm function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lm}}
}
\examples{
## Original function to interface
lm(conc ~ uptake, CO2)

## The interface reverses the order of data and formula
ntbt_lm(CO2, conc ~ uptake)

## so it can be used easily in a pipeline.
library(magrittr)
CO2 \%>\%
  ntbt_lm(conc ~ uptake)

CO2 \%>\%
  ntbt_lm(conc ~ uptake) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lm}
\keyword{lm}
