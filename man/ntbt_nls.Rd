\name{ntbt_nls}
\alias{ntbt_nls}
\title{Interface to nls for data science pipelines.}
\description{
  \code{ntbt_nls} is an interface to \code{stats::\link{nls}}
  (Nonlinear Least Squares) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_nls(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{nls}}.}
}
\details{
  ntbt_nls reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{nls}.
}
\value{
  Object returned by \code{\link{nls}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{nls}}
}
\examples{
## Original function to interface
nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase)

## The interface reverses the order of data and formula
ntbt_nls(data = DNase, density ~ SSlogis(log(conc), Asym, xmid, scal))

## so it can be used easily in a pipeline.
library(magrittr)
DNase \%>\%
  ntbt_nls(density ~ SSlogis(log(conc), Asym, xmid, scal))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_nls}
\keyword{nls}
