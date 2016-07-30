\name{ntbt_getInitial}
\alias{ntbt_getInitial}
\title{Interface to getInitial for data science pipelines.}
\description{
  \code{ntbt_getInitial} is an interface to \code{stats::\link{getInitial}}
  (Get Initial Parameter Estimates) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_getInitial(data, object, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{object}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{getInitial}}.}
}
\details{
  ntbt_getInitial reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{getInitial}.
}
\value{
  Object returned by \code{\link{getInitial}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{getInitial}}
}
\examples{
PurTrt <- Puromycin[ Puromycin$state == "treated", ]

## Original function to interface
getInitial(rate ~ SSmicmen( conc, Vm, K ), PurTrt)

## The interface reverses the order of data and formula
ntbt_getInitial(PurTrt, rate ~ SSmicmen( conc, Vm, K ))

## so it can be used easily in a pipeline.
library(magrittr)
PurTrt \%>\%
  ntbt_getInitial(rate ~ SSmicmen( conc, Vm, K ))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_getInitial}
\keyword{getInitial}
