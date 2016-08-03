\name{ntbt_pairs}
\alias{ntbt_pairs}
\title{Interface to pairs for data science pipelines.}
\description{
  \code{ntbt_pairs} is an interface to \code{graphics::\link{pairs}}
  (Scatterplot Matrices) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_pairs(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{pairs}}.}
}
\details{
  ntbt_pairs calls \code{pairs}.
}
\value{
  Object returned by \code{\link{pairs}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{pairs}}
}
\examples{
## Original function to interface
pairs(~ Fertility + Education + Catholic, data = swiss,
      subset = Education < 20,
      main = "Swiss data, Education < 20")

## The interface reverses the order of data and formula
ntbt_pairs(data = swiss, ~ Fertility + Education + Catholic,
           subset = Education < 20,
           main = "Swiss data, Education < 20")

## so it can be used easily in a pipeline.
library(magrittr)
swiss \%>\%
  ntbt_pairs(~ Fertility + Education + Catholic,
             subset = Education < 20,
             main = "Swiss data, Education < 20")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_pairs}
\keyword{pairs}
