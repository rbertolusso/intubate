\name{ntbt_survConcordance}
\alias{ntbt_survConcordance}
\title{Interface to survConcordance for data science pipelines.}
\description{
  \code{ntbt_survConcordance} is an interface to \code{survival::\link{survConcordance}}
  (Compute a concordance measure) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_survConcordance(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{survConcordance}}.}
}
\details{
  ntbt_survConcordance calls \code{\link{survConcordance}}.
}
\value{
  Object returned by \code{\link{survConcordance}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{survConcordance}}
}
\examples{
\dontrun{
library(survival)

## Original function to interface
survConcordance(Surv(time, status) ~ age, data=lung)

## The interface reverses the order of data and formula
ntbt_survConcordance(data=lung, Surv(time, status) ~ age)

## so it can be used easily in a pipeline.
library(magrittr)
lung \%>\%
  ntbt_survConcordance(Surv(time, status) ~ age)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_survConcordance}
\keyword{survConcordance}
