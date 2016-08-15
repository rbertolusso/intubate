\name{gss}
\alias{ntbt_gssanova}
\alias{ntbt_gssanova0}
\alias{ntbt_gssanova1}
\alias{ntbt_ssanova}
\alias{ntbt_ssanova0}
\alias{ntbt_ssanova9}
\alias{ntbt_sscden}
\alias{ntbt_sscden1}
\alias{ntbt_sscox}
\alias{ntbt_ssden}
\alias{ntbt_ssden1}
\alias{ntbt_sshzd}
\alias{ntbt_ssllrm}
\title{Interfaces for gss package for data science pipelines.}
\description{
  Interfaces to \code{gss} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gssanova(data, ...)
ntbt_gssanova0(data, ...)
ntbt_gssanova1(data, ...)
ntbt_ssanova(data, ...)
ntbt_ssanova0(data, ...)
ntbt_ssanova9(data, ...)
ntbt_sscden(data, ...)
ntbt_sscden1(data, ...)
ntbt_sscox(data, ...)
ntbt_ssden(data, ...)
ntbt_ssden1(data, ...)
ntbt_sshzd(data, ...)
ntbt_ssllrm(data, ...)
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
library(gss)


## ntbt_gssanova: Fitting Smoothing Spline ANOVA Models with Non-Gaussian Responses
data(bacteriuria)

## Original function to interface
gssanova(infect ~ trt + time, family="binomial", data = bacteriuria,
         id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)
gssanova0(infect ~ trt + time, family="binomial", data = bacteriuria)
gssanova1(infect ~ trt + time, family="binomial", data = bacteriuria,
         id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)

## The interface puts data as first parameter
ntbt_gssanova(bacteriuria, infect ~ trt + time, family="binomial",
              id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)
ntbt_gssanova0(bacteriuria, infect ~ trt + time, family="binomial")
ntbt_gssanova1(bacteriuria, infect ~ trt + time, family="binomial",
               id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)

## so it can be used easily in a pipeline.
bacteriuria \%>\%
  ntbt_gssanova(infect ~ trt + time, family="binomial",
                id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)
bacteriuria \%>\%
  ntbt_gssanova0(infect ~ trt + time, family="binomial")
bacteriuria \%>\%
  ntbt_gssanova1(infect ~ trt + time, family="binomial",
                 id.basis = (1:820)[bacteriuria$id %in% c(3,38)], random = ~ 1 | id)


## ntbt_ssanova: Fitting Smoothing Spline ANOVA Models
data(nox)

## Original function to interface
ssanova(log10(nox) ~ comp*equi, data = nox)
ssanova0(log10(nox) ~ comp*equi, data = nox)

## The interface puts data as first parameter
ntbt_ssanova(nox, log10(nox) ~ comp*equi)
ntbt_ssanova0(nox, log10(nox) ~ comp*equi)

## so it can be used easily in a pipeline.
nox \%>\%
  ntbt_ssanova(log10(nox) ~ comp*equi)
nox \%>\%
  ntbt_ssanova0(log10(nox) ~ comp*equi)


## ntbt_ssanova9: Fitting Smoothing Spline ANOVA Models with Correlated Data
x <- runif(100); y <- 5 + 3*sin(2*pi*x) + rnorm(x)
dta <- data.frame(x, y)

## Original function to interface
ssanova9(y ~ x, data = dta, cov = list("arma", c(1, 0)))

## The interface puts data as first parameter
ntbt_ssanova9(dta, y ~ x, cov = list("arma", c(1, 0)))

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_ssanova9(y ~ x, cov = list("arma", c(1, 0)))


## ntbt_sscden: Estimating Conditional Probability Density Using Smoothing Splines
data(penny)

## Original function to interface
set.seed(5732)
sscden(~ year*mil, ~ mil, data = penny, ydomain = data.frame(mil=c(49, 61)))
sscden1(~ year*mil, ~ mil, data = penny, ydomain = data.frame(mil=c(49, 61)))

## The interface puts data as first parameter
set.seed(5732)
ntbt_sscden(penny, ~ year*mil, ~ mil, ydomain = data.frame(mil=c(49, 61)))
ntbt_sscden1(penny, ~ year*mil, ~ mil, ydomain = data.frame(mil=c(49, 61)))

## so it can be used easily in a pipeline.
set.seed(5732)
penny \%>\%
  ntbt_sscden(~ year*mil, ~ mil, ydomain = data.frame(mil=c(49, 61)))
penny \%>\%
  ntbt_sscden1(~ year*mil, ~ mil, ydomain = data.frame(mil=c(49, 61)))


## ntbt_sscox: Estimating Relative Risk Using Smoothing Splines
data(stan)

## Original function to interface
sscox(Surv(futime, status) ~ age, data = stan)

## The interface puts data as first parameter
ntbt_sscox(stan, Surv(futime, status) ~ age)

## so it can be used easily in a pipeline.
stan \%>\%
  ntbt_sscox(Surv(futime, status) ~ age)


## ntbt_ssden: Estimating Probability Density Using Smoothing Splines
data(aids)
## rectangular quadrature
quad.pt <- expand.grid(incu=((1:40)-.5)/40*100,infe=((1:40)-.5)/40*100)
quad.pt <- quad.pt[quad.pt$incu<=quad.pt$infe,]
quad.wt <- rep(1,nrow(quad.pt))
quad.wt[quad.pt$incu==quad.pt$infe] <- .5
quad.wt <- quad.wt/sum(quad.wt)*5e3

## Original function to interface
ssden(~ incu + infe, data = aids, subset = age >= 60,
      domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
      quad = list(pt = quad.pt, wt = quad.wt))
ssden1(~ incu + infe, data = aids, subset = age >= 60,
       domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
       quad = list(pt = quad.pt, wt = quad.wt))

## The interface puts data as first parameter
ntbt_ssden(aids, ~ incu + infe, subset = age >= 60,
           domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
           quad = list(pt = quad.pt, wt = quad.wt))
ntbt_ssden1(aids, ~ incu + infe, subset = age >= 60,
            domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
            quad = list(pt = quad.pt, wt = quad.wt))

## so it can be used easily in a pipeline.
aids \%>\%
  ntbt_ssden(~ incu + infe, subset = age >= 60,
             domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
             quad = list(pt = quad.pt, wt = quad.wt))
aids \%>\%
  ntbt_ssden1(~ incu + infe, subset = age >= 60,
              domain = data.frame(incu = c(0, 100), infe=c(0, 100)),
              quad = list(pt = quad.pt, wt = quad.wt))


## ntbt_sshzd: Estimating Hazard Function Using Smoothing Splines
data(gastric)

## Original function to interface
sshzd(Surv(futime, status) ~ futime*trt, data = gastric)

## The interface puts data as first parameter
ntbt_sshzd(gastric, Surv(futime, status) ~ futime*trt)

## so it can be used easily in a pipeline.
gastric \%>\%
  ntbt_sshzd(Surv(futime, status) ~ futime*trt)


## ntbt_ssllrm: Fitting Smoothing Spline Log-Linear Regression Models
test <- function(x)
        {.3*(1e6*(x^11*(1-x)^6)+1e4*(x^3*(1-x)^10))-2}
x <- (0:100)/100
p <- 1-1/(1+exp(test(x)))
y <- rbinom(x,3,p)
y1 <- as.ordered(y)
y2 <- as.factor(rbinom(x,1,p))

dta <- data.frame(x, y1, y2)

## Original function to interface
ssllrm(~ y1*y2*x, ~ y1 + y2, data = dta)

## The interface puts data as first parameter
ntbt_ssllrm(dta, ~ y1*y2*x, ~ y1 + y2)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_ssllrm(~ y1*y2*x, ~ y1 + y2)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gss}
\keyword{gssanova}
\keyword{gssanova0}
\keyword{gssanova1}
\keyword{ssanova}
\keyword{ssanova0}
\keyword{ssanova9}
\keyword{sscden}
\keyword{sscden1}
\keyword{sscox}
\keyword{ssden}
\keyword{ssden1}
\keyword{sshzd}
\keyword{ssllrm}
