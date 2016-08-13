\name{robustbase}
\alias{ntbt_adjbox}
\alias{ntbt_glmrob}
\alias{ntbt_lmrob}
\alias{ntbt_ltsReg}
\alias{ntbt_nlrob}
\title{Interfaces for robustbase package for data science pipelines.}
\description{
  Interfaces to \code{robustbase} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_adjbox(data, ...)
ntbt_glmrob(data, ...)
ntbt_lmrob(data, ...)
ntbt_ltsReg(data, ...)
ntbt_nlrob(data, ...)
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
library(robustbase)


## ntbt_adjbox: Plot an Adjusted Boxplot for Skew Distributions
## Original function to interface
adjbox(len ~ dose, data = ToothGrowth)

## The interface puts data as first parameter
ntbt_adjbox(ToothGrowth, len ~ dose)

## so it can be used easily in a pipeline.
ToothGrowth \%>\%
  ntbt_adjbox(len ~ dose)


## ntbt_glmrob: Robust Fitting of Generalized Linear Models
data(carrots)

## Original function to interface
glmrob(cbind(success, total-success) ~ logdose + block,
       family = binomial, data = carrots, method= "Mqle",
       control= glmrobMqle.control(tcc=1.2))

## The interface puts data as first parameter
ntbt_glmrob(carrots, cbind(success, total-success) ~ logdose + block,
            family = binomial, method= "Mqle",
            control= glmrobMqle.control(tcc=1.2))

## so it can be used easily in a pipeline.
carrots \%>\%
  ntbt_glmrob(cbind(success, total-success) ~ logdose + block,
              family = binomial, method= "Mqle",
              control= glmrobMqle.control(tcc=1.2))


## ntbt_lmrob: MM-type Estimators for Linear Regression
data(coleman)

## Original function to interface
set.seed(0)
lmrob(Y ~ ., data = coleman, setting = "KS2011")

## The interface puts data as first parameter
ntbt_lmrob(coleman, Y ~ ., setting = "KS2011")

## so it can be used easily in a pipeline.
coleman \%>\%
  ntbt_lmrob(Y ~ ., setting = "KS2011")


## ntbt_ltsReg: Least Trimmed Squares Robust (High Breakdown) Regression
data(stackloss)

## Original function to interface
ltsReg(stack.loss ~ ., data = stackloss)

## The interface puts data as first parameter
ntbt_ltsReg(stackloss, stack.loss ~ .)

## so it can be used easily in a pipeline.
stackloss \%>\%
  ntbt_ltsReg(stack.loss ~ .)


## ntbt_nlrob: Robust Fitting of Nonlinear Regression Models
DNase1 <- DNase[ DNase$Run == 1, ]

## Original function to interface
nlrob(density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
      data = DNase1, trace = TRUE,
      start = list( Asym = 3, xmid = 0, scal = 1 ))

## The interface puts data as first parameter
ntbt_nlrob(DNase1, density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
           trace = TRUE,
           start = list( Asym = 3, xmid = 0, scal = 1 ))

## so it can be used easily in a pipeline.
DNase1 \%>\%
  ntbt_nlrob(density ~ Asym/(1 + exp(( xmid - log(conc) )/scal ) ),
             trace = TRUE,
             start = list( Asym = 3, xmid = 0, scal = 1 ))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{robustbase}
\keyword{adjbox}
\keyword{glmrob}
\keyword{lmrob}
\keyword{ltsReg}
\keyword{nlrob}
