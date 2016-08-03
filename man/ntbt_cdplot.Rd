\name{ntbt_cdplot}
\alias{ntbt_cdplot}
\title{Interface to cdplot for data science pipelines.}
\description{
  \code{ntbt_cdplot} is an interface to \code{graphics::\link{cdplot}}
  (Conditional Density Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cdplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{cdplot}}.}
}
\details{
  ntbt_cdplot calls \code{cdplot}.
}
\value{
  Object returned by \code{\link{cdplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{cdplot}}
}
\examples{
## NASA space shuttle o-ring failures
oring <- data.frame(
  fail = factor(c(2, 2, 2, 2, 1, 1, 1, 1,
                  1, 1, 2, 1, 2, 1, 1, 1,
                  1, 2, 1, 1, 1, 1, 1),
               levels = 1:2, labels = c("no", "yes")),
  temperature = c(53, 57, 58, 63, 66, 67, 67, 67,
                 68, 69, 70, 70, 70, 70, 72, 73,
                 75, 75, 76, 76, 78, 79, 81))

## Original function to interface
cdplot(fail ~ temperature, oring)
cdplot(fail ~ temperature, oring, bw = 2)
cdplot(fail ~ temperature, oring, bw = "SJ")

## The interface reverses the order of data and formula
ntbt_cdplot(oring, fail ~ temperature)
ntbt_cdplot(oring, fail ~ temperature, bw = 2)
ntbt_cdplot(oring, fail ~ temperature, bw = "SJ")

## so it can be used easily in a pipeline.
library(magrittr)
oring \%>\%
  ntbt_cdplot(fail ~ temperature)
oring \%>\%
  ntbt_cdplot(fail ~ temperature, bw = 2)
oring \%>\%
  ntbt_cdplot(fail ~ temperature, bw = "SJ")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_cdplot}
\keyword{cdplot}
