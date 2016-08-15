\name{spBayes}
\alias{ntbt_bayesGeostatExact}
\alias{ntbt_bayesLMConjugate}
\alias{ntbt_spDynLM}
\title{Interfaces for spBayes package for data science pipelines.}
\description{
  Interfaces to \code{spBayes} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bayesGeostatExact(data, ...)
ntbt_bayesLMConjugate(data, ...)
ntbt_spDynLM(data, ...)
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
library(spBayes)

## NOTE: Just interfacing 3 functions as a proof of concept. However,
##       there are so many things declared and used, that I am not sure if
##       everything could be passed on a pipeline (perhaps with intuBags down the line).
##       I may get back to it later.

## ntbt_bayesGeostatExact: Simple Bayesian spatial linear model with fixed semivariogram parameters
data(FORMGMT.dat)

n <- nrow(FORMGMT.dat)
p <- 5 ##an intercept an four covariates

n.samples <- 50

phi <- 0.0012

coords <- cbind(FORMGMT.dat$Longi, FORMGMT.dat$Lat)
coords <- coords*(pi/180)*6378

beta.prior.mean <- rep(0, times=p)
beta.prior.precision <- matrix(0, nrow=p, ncol=p)

alpha <- 1/1.5

sigma.sq.prior.shape <- 2.0
sigma.sq.prior.rate <- 10.0


## Original function to interface
bayesGeostatExact(Y ~ X1 + X2 + X3 + X4, data = FORMGMT.dat,
                  n.samples = n.samples,
                  beta.prior.mean = beta.prior.mean,
                  beta.prior.precision = beta.prior.precision,
                  coords = coords, phi = phi, alpha = alpha,
                  sigma.sq.prior.shape = sigma.sq.prior.shape,
                  sigma.sq.prior.rate = sigma.sq.prior.rate)

## The interface puts data as first parameter
ntbt_bayesGeostatExact(FORMGMT.dat, Y ~ X1 + X2 + X3 + X4,
                       n.samples = n.samples,
                       beta.prior.mean = beta.prior.mean,
                       beta.prior.precision = beta.prior.precision,
                       coords = coords, phi = phi, alpha = alpha,
                       sigma.sq.prior.shape = sigma.sq.prior.shape,
                       sigma.sq.prior.rate = sigma.sq.prior.rate)

## so it can be used easily in a pipeline.
FORMGMT.dat \%>\%
  ntbt_bayesGeostatExact(Y ~ X1 + X2 + X3 + X4,
                         n.samples = n.samples,
                         beta.prior.mean = beta.prior.mean,
                         beta.prior.precision = beta.prior.precision,
                         coords = coords, phi = phi, alpha = alpha,
                         sigma.sq.prior.shape = sigma.sq.prior.shape,
                         sigma.sq.prior.rate = sigma.sq.prior.rate)




## ntbt_bayesLMConjugate: Simple Bayesian linear model via the Normal/inverse-Gamma conjugate
n <- nrow(FORMGMT.dat)
p <- 7 ##an intercept and six covariates

n.samples <- 500

## Below we demonstrate the conjugate function in the special case
## with improper priors. The results are the same as for the above,
## up to MC error. 
beta.prior.mean <- rep(0, times=p)
beta.prior.precision <- matrix(0, nrow=p, ncol=p)

prior.shape <- -p/2
prior.rate <- 0

## Original function to interface
bayesLMConjugate(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = FORMGMT.dat,
                 n.samples, beta.prior.mean,
                 beta.prior.precision,
                 prior.shape, prior.rate)

## The interface puts data as first parameter
ntbt_bayesLMConjugate(FORMGMT.dat, Y ~ X1 + X2 + X3 + X4 + X5 + X6,
                      n.samples, beta.prior.mean,
                      beta.prior.precision,
                      prior.shape, prior.rate)

## so it can be used easily in a pipeline.
FORMGMT.dat \%>\%
  ntbt_bayesLMConjugate(Y ~ X1 + X2 + X3 + X4 + X5 + X6,
                        n.samples, beta.prior.mean,
                        beta.prior.precision,
                        prior.shape, prior.rate)


## ntbt_spDynLM: Function for fitting univariate Bayesian dynamic
##               space-time regression models
data("NETemp.dat")
ne.temp <- NETemp.dat

set.seed(1)

##take a chunk of New England
ne.temp <- ne.temp[ne.temp[,"UTMX"] > 5500000 & ne.temp[,"UTMY"] > 3000000,]

##subset first 2 years (Jan 2000 - Dec. 2002)
y.t <- ne.temp[,4:27]
N.t <- ncol(y.t) ##number of months
n <- nrow(y.t) ##number of observation per months

##add some missing observations to illistrate prediction
miss <- sample(1:N.t, 10)
holdout.station.id <- 5
y.t.holdout <- y.t[holdout.station.id, miss]
y.t[holdout.station.id, miss] <- NA

##scale to km
coords <- as.matrix(ne.temp[,c("UTMX", "UTMY")]/1000)
max.d <- max(iDist(coords))

##set starting and priors
p <- 2 #number of regression parameters in each month

starting <- list("beta"=rep(0,N.t*p), "phi"=rep(3/(0.5*max.d), N.t),
                 "sigma.sq"=rep(2,N.t), "tau.sq"=rep(1, N.t),
                 "sigma.eta"=diag(rep(0.01, p)))

tuning <- list("phi"=rep(5, N.t)) 

priors <- list("beta.0.Norm"=list(rep(0,p), diag(1000,p)),
               "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
               "sigma.sq.IG"=list(rep(2,N.t), rep(10,N.t)),
               "tau.sq.IG"=list(rep(2,N.t), rep(5,N.t)),
               "sigma.eta.IW"=list(2, diag(0.001,p)))

##make symbolic model formula statement for each month
mods <- lapply(paste(colnames(y.t),'elev',sep='~'), as.formula)

n.samples <- 10 # in original example it is 2000.

## Original function to interface
spDynLM(mods, data=cbind(y.t,ne.temp[,"elev",drop=FALSE]), coords=coords,
        starting=starting, tuning=tuning, priors=priors, get.fitted =TRUE,
        cov.model="exponential", n.samples=n.samples, n.report=25) 

## The interface puts data as first parameter
ntbt_spDynLM(cbind(y.t,ne.temp[,"elev",drop=FALSE]), mods, coords=coords,
             starting=starting, tuning=tuning, priors=priors, get.fitted =TRUE,
             cov.model="exponential", n.samples=n.samples, n.report=25) 

## so it can be used easily in a pipeline.
cbind(y.t,ne.temp[,"elev",drop=FALSE]) \%>\%
  ntbt_spDynLM(mods, coords=coords,
               starting=starting, tuning=tuning, priors=priors, get.fitted =TRUE,
               cov.model="exponential", n.samples=n.samples, n.report=25) 
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{spBayes}
\keyword{bayesGeostatExact}
\keyword{bayesLMConjugate}
\keyword{spDynLM}