\name{ntbt_cor.test}
\alias{ntbt_cor.test}
\title{Interface to cor.test for data science pipelines.}
\description{
  \code{ntbt_cor.test} is an interface to \code{stats::\link{cor.test}}
  (Test for Association/Correlation Between Paired Samples) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_cor.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{cor.test}}.}
}
\details{
  ntbt_cor.test calls \code{cor.test}.
}
\value{
  Object returned by cor.test function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{cor.test}}
}
\examples{
## Original function to interface
cor.test(~ CONT + INTG, data = USJudgeRatings)

## The interface reverses the order of data and formula
ntbt_cor.test(data = USJudgeRatings, ~ CONT + INTG)

## so it can be used easily in a pipeline.
library(magrittr)
USJudgeRatings \%>\%
  ntbt_cor.test(~ CONT + INTG)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_cor.test}
\keyword{cor.test}
