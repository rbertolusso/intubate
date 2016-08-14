\name{glmx}
\alias{ntbt_glmx}
\alias{ntbt_hetglm}
\title{Interfaces for glmx package for data science pipelines.}
\description{
  Interfaces to \code{glmx} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_glmx(data, ...)
ntbt_hetglm(data, ...)
}
\arguments{
  \item{data}{data frame, tibble, list, ...}
  \item{...}{Other arguments passed to the corresponding interfaced function.}
}
\details{
  Interfaces call their corresponding interfaced function.
}
\value{
  Object returned by interfaced function.
}
\author{
Roberto Bertolusso
}
\examples{
\dontrun{
library(intubate)
library(magrittr)
library(glmx)

## ntbt_glmx: Generalized Linear Models with Extra Parameters
set.seed(1)
d <- data.frame(x = runif(200, -1, 1))
d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
require("MASS")

## Original function to interface
glmx(y ~ x, data = d, family = negative.binomial, xlink = "log", xstart = 0)

## The interface puts data as first parameter
ntbt_glmx(d, y ~ x, family = negative.binomial, xlink = "log", xstart = 0)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_glmx(y ~ x, family = negative.binomial, xlink = "log", xstart = 0)


## ntbt_hetglm: Heteroskedastic Binary Response GLMs
n <- 200
x <- rnorm(n)
ystar <- 1 + x +  rnorm(n, sd = exp(x))
y  <- factor(ystar > 0) 
dta <- data.frame(x, y)

## Original function to interface
hetglm(y ~ x | 1, data = dta)

## The interface puts data as first parameter
ntbt_hetglm(dta, y ~ x | 1)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_hetglm(y ~ x | 1)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{glmx}
\keyword{hetglm}
