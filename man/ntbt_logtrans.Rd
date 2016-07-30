\name{ntbt_logtrans}
\alias{ntbt_logtrans}
\title{Interface to logtrans for data science pipelines.}
\description{
  \code{ntbt_logtrans} is an interface to \code{MASS::\link{logtrans}}
  (Get Initial Parameter Estimates) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_logtrans(data, object, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{object}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{logtrans}}.}
}
\details{
  ntbt_logtrans reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{logtrans}.
}
\value{
  Object returned by \code{\link{logtrans}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{logtrans}}
}
\examples{
library(MASS)

## Original function to interface
logtrans(Days ~ Age*Sex*Eth*Lrn, data = quine,
         alpha = seq(0.75, 6.5, len=20))

## The interface reverses the order of data and formula
ntbt_logtrans(data = quine, Days ~ Age*Sex*Eth*Lrn,
              alpha = seq(0.75, 6.5, len=20))

## so it can be used easily in a pipeline.
library(magrittr)
quine \%>\%
  ntbt_logtrans(Days ~ Age*Sex*Eth*Lrn,
                alpha = seq(0.75, 6.5, len=20))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_logtrans}
\keyword{logtrans}
