\name{hdm}
\alias{ntbt_rlasso}
\alias{ntbt_rlassoATE}
\alias{ntbt_rlassoATET}
\alias{ntbt_rlassoLATE}
\alias{ntbt_rlassoLATET}
\alias{ntbt_rlassoEffects}
\alias{ntbt_rlassoIV}
\alias{ntbt_rlassologit}
%\alias{ntbt_tsls}
\title{Interfaces for hdm package for data science pipelines.}
\description{
  Interfaces to \code{hdm} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_rlasso(data, ...)
ntbt_rlassoATE(data, ...)
ntbt_rlassoATET(data, ...)
ntbt_rlassoLATE(data, ...)
ntbt_rlassoLATET(data, ...)
ntbt_rlassoEffects(data, ...)
ntbt_rlassoIV(data, ...)
ntbt_rlassologit(data, ...)
# ntbt_tsls(data, ...)           ## Already defined in sem
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
library(hdm)


## ntbt_rlasso: rlasso: Function for Lasso estimation under homoscedastic
##                      and heteroscedastic non-Gaussian disturbances
set.seed(1)
n <- 100 #sample size
p <- 100 # number of variables
s <- 3 # nubmer of variables with non-zero coefficients
X <- Xnames <- matrix(rnorm(n*p), ncol=p)
colnames(Xnames) <- paste("V", 1:p, sep="")
beta <- c(rep(5,s), rep(0,p-s))
Y <- X \%*\% beta + rnorm(n)

dta <- list(Y = Y, Xnames = Xnames)
rm(Y, Xnames)

## Original function to interface
rlasso(Y ~ Xnames, data = dta)

## The interface puts data as first parameter
ntbt_rlasso(dta, Y ~ Xnames)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rlasso(Y ~ Xnames)


## Functions for estimation of treatment effects
## ntbt_rlassoATE, ntbt_rlassoATET, ntbt_rlassoLATE, ntbt_rlassoLATET
## do not have examples of use in help.

## Original function to interface
## The interface puts data as first parameter
## so it can be used easily in a pipeline.



## ntbt_rlassoEffects: rigorous Lasso for Linear Models: Inference
set.seed(1)
n <- 100 #sample size
p <- 100 # number of variables
s <- 3 # nubmer of non-zero variables
X <- matrix(rnorm(n*p), ncol=p)
colnames(X) <- paste("X", 1:p, sep="")
beta <- c(rep(3,s), rep(0,p-s))
y <- 1 + X \%*\% beta + rnorm(n)
data <- data.frame(cbind(y,X))
colnames(data)[1] <- "y"
fm <- paste("y ~", paste(colnames(X), collapse="+"))
fm <- as.formula(fm)                 
rm(y, X)

## Original function to interface
rlassoEffects(fm, I = ~ X1 + X2 + X3 + X50, data=data)

## The interface puts data as first parameter
ntbt_rlassoEffects(data, fm, I = ~ X1 + X2 + X3 + X50)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_rlassoEffects(fm, I = ~ X1 + X2 + X3 + X50)



## ntbt_rlassoIV: Post-Selection and Post-Regularization Inference
##                in Linear Models with Many Controls and Instruments
## The example uses non-formula variant. Please see note below about
## possible problem.
data(EminentDomain)
dta <- list(z = EminentDomain$logGDP$z, # instruments
            x = EminentDomain$logGDP$x, # exogenous variables
            y = EminentDomain$logGDP$y, # outcome varialbe
            d = EminentDomain$logGDP$d) # treatment / endogenous variable
str(dta)
## Original function to interface
attach(dta)
rlassoIV(x=x, d=d, y=y, z=z, select.X=FALSE, select.Z=TRUE) 
detach()

## The interface puts data as first parameter
## NOTE: BE CAREFUL (in general in situations as follow)
## The parameter name "d" in this function can result in a nightmare
## (it got me scratching my head for quite a bit).
## In fact, if you call with parameter names (but not naming data)
## call the following version (commented out)
# ntbt_rlassoIV(dta, x=x, d=d, y=y, z=z, select.X=FALSE, select.Z=TRUE)
## there will be an error, as R will expand "d" to "data", and use
## its info (d) instead of dta.
## Right now I am not sure how to manage this situation and avoid
## that unwanted expansion. I will get back to this later.
## To avoid problems you should specify "data" as below
ntbt_rlassoIV(data=dta, x=x, d=d, y=y, z=z, select.X=FALSE, select.Z=TRUE)
## but of course this beats the purpose (we do not want to name "data"),
## and you *cannot* do it in the pipeline version (as you do not include data
## in your call).
## SOLUTION. In cases of unfortunate parameter names: "d", "da", "dat",
## you need to make sure that that parameter is sent by position AND unnamed
ntbt_rlassoIV(dta, x=x, d, y=y, z=z, select.X=FALSE, select.Z=TRUE)
## In general, required data is sent unnamed and by position, like
ntbt_rlassoIV(dta, x, d, y, z, select.X = FALSE, select.Z = TRUE)
## and this would have been what I would have done if I would
## (have had the ability to) produce this example.
## But this is how the example was provided, giving an opportunity to
## uncover this potentially unpleasant situation.

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rlassoIV(x, d, y, z, select.X = FALSE, select.Z = TRUE)


## ntbt_rlassologit: Function for logistic Lasso estimation
library(hdm)
## DGP
set.seed(2)
n <- 250
p <- 100
px <- 10
X <- matrix(rnorm(n*p), ncol=p)
beta <- c(rep(2,px), rep(0,p-px))
intercept <- 1
P <- exp(intercept + X \%*\% beta)/(1+exp(intercept + X \%*\% beta))
y <- rbinom(n, size=1, prob=P)
dta <- list(y = y, X = X)
rm(y, X)

## Original function to interface
rlassologit(y ~ X, dta)

## The interface puts data as first parameter
ntbt_rlassologit(dta, y ~ X)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rlassologit(y ~ X)



## ntbt_tsls: Two-Stage Least Squares Estimation (TSLS)
## No example provided
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{hdm}
\keyword{rlasso}
\keyword{rlassoATE}
\keyword{rlassoATET}
\keyword{rlassoLATE}
\keyword{rlassoLATET}
\keyword{rlassoEffects}
\keyword{rlassoIV}
\keyword{rlassologit}
\keyword{tsls}
