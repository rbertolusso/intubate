\name{ntbt_model.frame}
\alias{ntbt_model.frame}
\title{Interface to model.frame for data science pipelines.}
\description{
  \code{ntbt_model.frame} is an interface to \code{stats::\link{model.frame}}
  (Extracting the Model Frame from a Formula or Fit) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_model.frame(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{model.frame}}.}
}
\details{
  ntbt_model.frame calls \code{model.frame}.
}
\value{
  Object returned by \code{\link{model.frame}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{model.frame}}
}
\examples{
## Original function to interface
model.frame(dist ~ speed, data = cars)

## The interface reverses the order of data and formula
ntbt_model.frame(data = cars, dist ~ speed)

## so it can be used easily in a pipeline.
library(magrittr)
cars \%>\%
  ntbt_model.frame(dist ~ speed)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_model.frame}
\keyword{model.frame}
