\name{arm}
\alias{ntbt_bayesglm}
\alias{ntbt_bayespolr}
\title{Interfaces for arm package for data science pipelines.}
\description{
  Interfaces to \code{arm} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bayesglm(data, ...)
ntbt_bayespolr(data, ...)
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
library(arm)

## ntbt_bayesglm: Bayesian generalized linear models
n <- 100
x1 <- rnorm (n)
x2 <- rbinom (n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
y <- rbinom(n, 1, invlogit(b0+b1*x1+b2*x2))

dta <- data.frame(y, x1, x2)

## Original function to interface
bayesglm(y ~ x1 + x2, family = binomial(link="logit"), data = dta,
         prior.scale = Inf, prior.df = Inf)

## The interface puts data as first parameter
ntbt_bayesglm(dta, y ~ x1 + x2, family = binomial(link="logit"),
              prior.scale = Inf, prior.df = Inf)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_bayesglm(y ~ x1 + x2, family = binomial(link="logit"),
                prior.scale = Inf, prior.df = Inf)


## ntbt_bayespolr: Bayesian Ordered Logistic or Probit Regression
## Original function to interface
bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
          prior.scale = Inf, prior.df = Inf)

## The interface puts data as first parameter
ntbt_bayespolr(housing, Sat ~ Infl + Type + Cont, weights = Freq,
               prior.scale = Inf, prior.df = Inf)

## so it can be used easily in a pipeline.
housing \%>\%
  ntbt_bayespolr(Sat ~ Infl + Type + Cont, weights = Freq,
                 prior.scale = Inf, prior.df = Inf)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{arm}
\keyword{bayesglm}
\keyword{bayespolr}
