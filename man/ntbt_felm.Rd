\name{ntbt_felm}
\alias{ntbt_felm}
\title{Interface to felm for data science pipelines.}
\description{
  \code{ntbt_felm} is an interface to \code{MASS::\link{felm}}
  (Fit a linear model with multiple group fixed effects) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_felm(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{felm}}.}
}
\details{
  ntbt_felm reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{felm}}.
}
\value{
  Object returned by \code{\link{felm}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lfe}}
}
\examples{
library(lfe)
oldopts <- options(lfe.threads=1)

## create covariates
data <- within(list(x = rnorm(1000)), {
  x2 <- rnorm(length(x))
  ## individual and firm
  id <- factor(sample(20,length(x),replace=TRUE))
  firm <- factor(sample(13,length(x),replace=TRUE))
  ## left hand side
  u <- rnorm(length(x))
  y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u
  x3 <- rnorm(length(x))
  x4 <- sample(12,length(x),replace=TRUE)
  Q <- 0.3*x3 + x + 0.2*x2 + id.eff[id] + 0.3*log(x4) - 0.3*y + rnorm(length(x),sd=0.3)
  W <- 0.7*x3 - 2*x + 0.1*x2 - 0.7*id.eff[id] + 0.8*cos(x4) - 0.2*y+ rnorm(length(x),sd=0.6)
  # add them to the outcome
  y <- y + Q + W
})

## effects for them
id.eff <- rnorm(nlevels(data$id))
firm.eff <- rnorm(nlevels(data$firm))

## NOTE: the original function converts the list to a data.frame.
##       This is a pity, because if not, also id.eff and firm.eff
##       could be on the list and be sent in data to felm.

## Original function to interface
est <- felm(y ~ x + x2 | id + firm, data)
summary(est)

ivest <- felm(y ~ x + x2 | id + firm | (Q | W ~ x3 + factor(x4)), data)
summary(ivest, robust = TRUE)
condfstat(ivest)

## The interface reverses the order of data and formula
est <- ntbt_felm(data, y ~ x + x2 | id + firm)
summary(est)

ivest <- ntbt_felm(data, y ~ x + x2 | id + firm | (Q | W ~ x3 + factor(x4)))
summary(ivest, robust = TRUE)
condfstat(ivest)

## so it can be used easily in a pipeline.
library(magrittr)
data \%>\%
  ntbt_felm(y ~ x + x2 | id + firm) \%>\%
  summary()
  
data \%>\%
  ntbt_felm(data, y ~ x + x2 | id + firm | (Q | W ~ x3 + factor(x4))) \%T>\%
  summary(robust = TRUE) \%>\%
  condfstat()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_felm}
\keyword{felm}
