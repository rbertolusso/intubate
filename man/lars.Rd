\name{lars}
\alias{ntbt_lars}
\alias{ntbt_cv.lars}
\title{Interfaces for lars package for data science pipelines.}
\description{
  Interfaces to \code{lars} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_lars(data, ...)
ntbt_cv.lars(data, ...)
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
library(lars)


library(ISLR)
data("Hitters")
Hitters <- na.omit(Hitters)

dta <- list(x = model.matrix(Salary ~ ., Hitters)[, -1],  ## Remove intercept
            y = model.frame(Salary ~ ., Hitters)[, 1])

## ntbt_lars: Fits Least Angle Regression, Lasso and Infinitesimal
##            Forward Stagewise regression models

## Original function to interface
attach(dta)
lasso <- lars(x, y)
plot(lasso)
detach()

## The interface puts data as first parameter
lasso <- ntbt_lars(dta, x, y)
plot(lasso)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_lars(x, y) \%>\%
  plot()


## ntbt_cv.lars: Computes K-fold cross-validated error curve for lars

## Original function to interface
set.seed(1)
attach(dta)
cv.lars(x, y)
detach()

## The interface puts data as first parameter
set.seed(1)
ntbt_cv.lars(dta, x, y)

## so it can be used easily in a pipeline.
set.seed(1)
dta \%>\%
  ntbt_cv.lars(x, y)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{lars}
\keyword{cv.lars}
