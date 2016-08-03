\name{ntbt_prcomp}
\alias{ntbt_prcomp}
\title{Interface to prcomp for data science pipelines.}
\description{
  \code{ntbt_prcomp} is an interface to \code{stats::\link{prcomp}}
  (Principal Components Analysis) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_prcomp(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{prcomp}}.}
}
\details{
  ntbt_prcomp calls \code{prcomp}.
}
\value{
  Object returned by \code{\link{prcomp}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{prcomp}}
}
\examples{
## Original function to interface
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)

## The interface reverses the order of data and formula
ntbt_prcomp(data = USArrests, ~ Murder + Assault + Rape, scale = TRUE)

## so it can be used easily in a pipeline.
library(magrittr)
USArrests \%>\%
  ntbt_prcomp(~ Murder + Assault + Rape, scale = TRUE)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_prcomp}
\keyword{prcomp}
