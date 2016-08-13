\name{quantreg}
\alias{ntbt_dynrq}
\alias{ntbt_KhmaladzeTest}
\alias{ntbt_nlrq}
\alias{ntbt_rq}
\alias{ntbt_rqProcess}
\alias{ntbt_rqss}
\title{Interfaces for quantreg package for data science pipelines.}
\description{
  Interfaces to \code{quantreg} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_dynrq(data, ...)
ntbt_KhmaladzeTest(data, ...)
ntbt_nlrq(data, ...)
ntbt_rq(data, ...)
ntbt_rqProcess(data, ...)
ntbt_rqss(data, ...)
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
library(quantreg)


## ntbt_dynrq: Dynamic Linear Quantile Regression
require(zoo)
data("UKDriverDeaths", package = "datasets")
dta <- data.frame(uk = log10(UKDriverDeaths))

## Original function to interface
dynrq(uk ~ L(uk, 1) + L(uk, 12), data = dta)

## The interface puts data as first parameter
ntbt_dynrq(dta, uk ~ L(uk, 1) + L(uk, 12))

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_dynrq(uk ~ L(uk, 1) + L(uk, 12))


## ntbt_KhmaladzeTest: Tests of Location and Location Scale Shift Hypotheses for Linear Models
data(barro)
## Original function to interface
KhmaladzeTest(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
              data = barro, taus = seq(.05,.95,by = .01))

## The interface puts data as first parameter
ntbt_KhmaladzeTest(barro, y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
                   taus = seq(.05,.95,by = .01))

## so it can be used easily in a pipeline.
barro \%>\%
  ntbt_KhmaladzeTest(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
                     taus = seq(.05,.95,by = .01))


## ntbt_nlrq: Function to compute nonlinear quantile regression estimates
Dat <- NULL; Dat$x <- rep(1:25, 20)
set.seed(1)
Dat$y <- SSlogis(Dat$x, 10, 12, 2)*rnorm(500, 1, 0.1)

## Original function to interface
nlrq(y ~ SSlogis(x, Asym, mid, scal), data = Dat, tau = 0.5, trace = TRUE)

## The interface puts data as first parameter
ntbt_nlrq(Dat, y ~ SSlogis(x, Asym, mid, scal), tau = 0.5, trace = TRUE)

## so it can be used easily in a pipeline.
Dat \%>\%
  ntbt_nlrq(y ~ SSlogis(x, Asym, mid, scal), tau = 0.5, trace = TRUE)


## ntbt_rq: Quantile Regression
data(stackloss)
dta <- data.frame(stack.loss, stack.x)

## Original function to interface
rq(stack.loss ~ stack.x, .5, data = dta)  # median (l1) regression  fit for the stackloss data. 

## The interface puts data as first parameter
ntbt_rq(dta, stack.loss ~ stack.x, .5)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rq(stack.loss ~ stack.x, .5)


## ntbt_rqProcess: Compute Standardized Quantile Regression Process
## Original function to interface
data(barro)
rqProcess(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
          data = barro, taus = seq(.05,.95,by = .01))

## The interface puts data as first parameter
ntbt_rqProcess(barro, y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
               taus = seq(.05,.95,by = .01))

## so it can be used easily in a pipeline.
barro \%>\%
  ntbt_rqProcess(y.net ~ lgdp2 + fse2 + gedy2 + Iy2 + gcony2, 
                 taus = seq(.05,.95,by = .01))


## ntbt_rqss: Additive Quantile Regression Smoothing
n <- 200
x <- sort(rchisq(n,4))
z <- x + rnorm(n)
y <- log(x)+ .1*(log(x))^2 + log(x)*rnorm(n)/4 + z
dta <- data.frame(x, y, z)

## Original function to interface
rqss(y ~ qss(x, constraint= "N") + z, data = dta)

## The interface puts data as first parameter
ntbt_rqss(dta, y ~ qss(x, constraint= "N") + z)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rqss(y ~ qss(x, constraint= "N") + z)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{quantreg}
\keyword{dynrq}
\keyword{KhmaladzeTest}
\keyword{nlrq}
\keyword{rq}
\keyword{rqProcess}
\keyword{rqss}
