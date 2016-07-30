\name{ntbt_alias}
\alias{ntbt_alias}
\title{Interface to alias for data science pipelines.}
\description{
  \code{ntbt_alias} is an interface to \code{stats::\link{alias}}
  (Find Aliases (Dependencies) in a Model) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_alias(data, object, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{object}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{alias}}.}
}
\details{
  ntbt_alias reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{alias}.
}
\value{
  Object returned by \code{\link{alias}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{alias}}
}
\examples{
## Original function to interface
alias(yield ~ block + N*P*K, npk)

## The interface reverses the order of data and formula
ntbt_alias(npk, yield ~ block + N*P*K)

## so it can be used easily in a pipeline.
library(magrittr)
npk \%>\%
  ntbt_alias(yield ~ block + N*P*K)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_alias}
\keyword{alias}
