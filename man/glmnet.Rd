\name{glmnet}
\alias{ntbt_glmnet}
\alias{ntbt_cv.glmnet}
\title{Interfaces for glmnet package for data science pipelines.}
\description{
  Interfaces to \code{glmnet} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_glmnet(data, ...)
ntbt_cv.glmnet(data, ...)
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
library(glmnet)


## NOTE: glmnet package does not implement formula interface. As I need it
##       for teaching purposes, this is the first pure non-formula library
##       included, as a proof of concept that not only the functions with
##       formula + data variant can be successfully interfaced to use in
##       a pipeline.


library(ISLR)
data("Hitters")
Hitters <- na.omit(Hitters)

dta <- list(x = model.matrix(Salary ~ ., Hitters)[, -1],  ## Remove intercept
            y = model.frame(Salary ~ ., Hitters)[, 1])
grid <- 10^seq(10, -2, length = 100)

## ntbt_glmnet: fit a GLM with lasso or elasticnet regularization

## Original function to interface
attach(dta)
## Ridge Regression
ridge <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge)

## The Lasso
lasso <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso)
detach()

## The interface puts data as first parameter
## Ridge Regression
ridge <- ntbt_glmnet(dta, x, y, alpha = 0, lambda = grid)
plot(ridge)

## The Lasso
lasso <- ntbt_glmnet(dta, x, y, alpha = 1, lambda = grid)
plot(lasso)

## so it can be used easily in a pipeline.
## Ridge Regression
dta \%>\%
  ntbt_glmnet(x, y, alpha = 0, lambda = grid) \%>\%
  plot()

## The Lasso
dta \%>\%
  ntbt_glmnet(x, y, alpha = 1, lambda = grid) \%>\%
  plot()


## ntbt_cv.glmnet: cv.glmnet Cross-validation for glmnet

## Original function to interface
attach(dta)
## Ridge Regression
cv.ridge <- cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

## The Lasso
cv.lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)
detach()

## The interface puts data as first parameter
## Ridge Regression
cv.ridge <- ntbt_cv.glmnet(dta, x, y, alpha = 0)
plot(cv.ridge)

## The Lasso
cv.lasso <- ntbt_cv.glmnet(dta, x, y, alpha = 1)
plot(cv.lasso)

## so it can be used easily in a pipeline.
## Ridge Regression
dta \%>\%
  ntbt_cv.glmnet(x, y, alpha = 0) \%>\%
  plot()

## The Lasso
dta \%>\%
  ntbt_cv.glmnet(x, y, alpha = 1) \%>\%
  plot()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{glmnet}
\keyword{cv.glmnet}
