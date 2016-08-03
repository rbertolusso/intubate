\name{ntbt_xtabs}
\alias{ntbt_xtabs}
\title{Interface to xtabs for data science pipelines.}
\description{
  \code{ntbt_xtabs} is an interface to \code{stats::\link{xtabs}}
  (Cross Tabulation) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_xtabs(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{xtabs}}.}
}
\details{
  ntbt_xtabs calls \code{\link{xtabs}}.
}
\value{
  Object returned by \code{\link{xtabs}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{xtabs}}, \code{\link{ntbt_aggregate}}
}
\examples{
## Original function to interface
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)

## The interface reverses the order of data and formula
ag <- ntbt_aggregate(ToothGrowth, len ~ ., mean)
ntbt_xtabs(ag, len ~ .)

## so it can be used easily in a pipeline.
library(magrittr)
ToothGrowth \%>\%
  ntbt_aggregate(len ~ ., mean) \%>\%
  ntbt_xtabs(len ~ .)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_aggregate}
\keyword{ntbt_xtabs}
\keyword{xtabs}
