\name{ntbt_spineplot}
\alias{ntbt_spineplot}
\title{Interface to spineplot for data science pipelines.}
\description{
  \code{ntbt_spineplot} is an interface to \code{graphics::\link{spineplot}}
  (Spine Plots and Spinograms) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_spineplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{spineplot}}.}
}
\details{
  ntbt_spineplot calls \code{spineplot}.
}
\value{
  Object returned by \code{\link{spineplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{spineplot}}
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
spineplot(fail ~ temperature, oring)
spineplot(fail ~ temperature, oring, breaks = 3)

## The interface reverses the order of data and formula
ntbt_spineplot(oring, fail ~ temperature)
ntbt_spineplot(oring, fail ~ temperature, breaks = 3)

## so it can be used easily in a pipeline.
library(magrittr)
oring \%>\%
  ntbt_spineplot(fail ~ temperature)
oring \%>\%
  ntbt_spineplot(fail ~ temperature, breaks = 3)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_spineplot}
\keyword{spineplot}
