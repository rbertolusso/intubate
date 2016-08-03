\name{ntbt_polr}
\alias{ntbt_polr}
\title{Interface to polr for data science pipelines.}
\description{
  \code{ntbt_polr} is an interface to \code{MASS::\link{polr}}
  (Ordered Logistic or Probit Regression) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_polr(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{polr}}.}
}
\details{
  ntbt_polr calls \code{\link{polr}}.
}
\value{
  Object returned by \code{\link{polr}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{polr}}
}
\examples{
op <- options(contrasts = c("contr.treatment", "contr.poly"))

library(MASS)

## Original function to interface
polr(Sat ~ Infl + Type + Cont, housing)

## The interface reverses the order of data and formula
ntbt_polr(housing, Sat ~ Infl + Type + Cont)

## so it can be used easily in a pipeline.
library(magrittr)
housing \%>\%
  ntbt_polr(Sat ~ Infl + Type + Cont)

options(op)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_polr}
\keyword{polr}
