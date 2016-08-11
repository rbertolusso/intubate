\name{rms}
\alias{ntbt_bj}
\alias{ntbt_cph}
\alias{ntbt_Glm}
\alias{ntbt_lrm}
\alias{ntbt_npsurv}
\alias{ntbt_ols}
\alias{ntbt_orm}
\alias{ntbt_psm}
\title{Interfaces for rms package for data science pipelines.}
\description{
  Interfaces to \code{rms} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bj(data, ...)
ntbt_cph(data, ...)
ntbt_Glm(data, ...)
ntbt_lrm(data, ...)
ntbt_npsurv(data, ...)
ntbt_ols(data, ...)
ntbt_orm(data, ...)
ntbt_psm(data, ...)
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
library(magrittr)
library(rms)

## ntbt_bj: Buckley-James Multiple Regression Model
set.seed(1)
ftime  <- 10*rexp(200)
stroke <- ifelse(ftime > 10, 0, 1)
ftime  <- pmin(ftime, 10)
units(ftime) <- "Month"
age <- rnorm(200, 70, 10)
hospital <- factor(sample(c('a','b'),200,TRUE))
dd <- datadist(age, hospital)
options(datadist = "dd")
data_bj <- data.frame(ftime, stroke, age, hospital)

## Original function to interface
bj(Surv(ftime, stroke) ~ rcs(age,5) + hospital, data_bj, x = TRUE, y = TRUE)

## The interface puts data as first parameter
f <- ntbt_bj(data_bj, Surv(ftime, stroke) ~ rcs(age,5) + hospital, x = TRUE, y = TRUE)
anova(f)

## so it can be used easily in a pipeline.
data_bj \%>\%
  ntbt_bj(Surv(ftime, stroke) ~ rcs(age,5) + hospital, x = TRUE, y = TRUE)


## ntbt_cph: Cox Proportional Hazards Model and Extensions
n <- 1000
set.seed(731)
age <- 50 + 12*rnorm(n)
label(age) <- "Age"
sex <- factor(sample(c('Male','Female'), n, 
              rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)
h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
dt <- -log(runif(n))/h
label(dt) <- 'Follow-up Time'
e <- ifelse(dt <= cens,1,0)
dt <- pmin(dt, cens)
units(dt) <- "Year"
dd <- datadist(age, sex)
options(datadist='dd')
S <- Surv(dt,e)

data_cph <- data.frame(S, age, sex)

## Original function to interface
cph(S ~ rcs(age,4) + sex, data_cph, x = TRUE, y = TRUE)

## The interface puts data as first parameter
ntbt_cph(data_cph, S ~ rcs(age,4) + sex, x = TRUE, y = TRUE)

## so it can be used easily in a pipeline.
data_cph \%>\%
  ntbt_cph(S ~ rcs(age,4) + sex, x = TRUE, y = TRUE)


## ntbt_Glm
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data_Glm <- data.frame(counts, outcome, treatment)

## Original function to interface
Glm(counts ~ outcome + treatment, family = poisson(), data = data_Glm)

## The interface puts data as first parameter
ntbt_Glm(data_Glm, counts ~ outcome + treatment, family = poisson())

## so it can be used easily in a pipeline.
data_Glm \%>\%
  ntbt_Glm(counts ~ outcome + treatment, family = poisson())


## ntbt_lrm: Logistic Regression Model
n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))
label(age)            <- 'Age'      # label is in Hmisc
label(cholesterol)    <- 'Total Cholesterol'
label(blood.pressure) <- 'Systolic Blood Pressure'
label(sex)            <- 'Sex'
units(cholesterol)    <- 'mg/dl'   # uses units.default in Hmisc
units(blood.pressure) <- 'mmHg'

#To use prop. odds model, avoid using a huge number of intercepts by
#grouping cholesterol into 40-tiles
ch <- cut2(cholesterol, g=40, levels.mean=TRUE) # use mean values in intervals
data_lrm <- data.frame(ch, age)

## Original function to interface
lrm(ch ~ age, data_lrm)

## The interface puts data as first parameter
ntbt_lrm(data_lrm, ch ~ age)

## so it can be used easily in a pipeline.
data_lrm \%>\%
  ntbt_lrm(ch ~ age)


## ntbt_npsurv: Nonparametric Survival Estimates for Censored Data
tdata <- data.frame(time   = c(1,1,1,2,2,2,3,3,3,4,4,4),
                    status = rep(c(1,0,2),4),
                    n      = c(12,3,2,6,2,4,2,0,2,3,3,5))
## Original function to interface
f <- npsurv(Surv(time, time, status, type = 'interval') ~ 1, data = tdata, weights = n)
plot(f, fun = 'event', xmax = 20, mark.time = FALSE, col = 2:3)

## The interface puts data as first parameter
f <- ntbt_npsurv(tdata, Surv(time, time, status, type = 'interval') ~ 1, weights = n)
plot(f, fun = 'event', xmax = 20, mark.time = FALSE, col = 2:3)

## so it can be used easily in a pipeline.
tdata \%>\%
  ntbt_npsurv(Surv(time, time, status, type = 'interval') ~ 1, weights = n) \%>\%
  plot(fun = 'event', xmax = 20, mark.time = FALSE, col = 2:3)


## ntbt_ols: Linear Model Estimation Using Ordinary Least Squares
set.seed(1)
x1 <- runif(200)
x2 <- sample(0:3, 200, TRUE)
distance <- (x1 + x2/3 + rnorm(200))^2
d <- datadist(x1, x2)
options(datadist="d")   # No d -> no summary, plot without giving all details
data_ols <- data.frame(distance, x1, x2)

## Original function to interface
ols(sqrt(distance) ~ rcs(x1, 4) + scored(x2), data_ols, x = TRUE)
 
## The interface puts data as first parameter
ntbt_ols(data_ols, sqrt(distance) ~ rcs(x1, 4) + scored(x2), x = TRUE)

## so it can be used easily in a pipeline.
data_ols \%>\%
  ntbt_ols(sqrt(distance) ~ rcs(x1, 4) + scored(x2), x = TRUE)


## ntbt_orm: Ordinal Regression Model
set.seed(1)
n <- 300
x1 <- c(rep(0,150), rep(1,150))
y <- rnorm(n) + 3 * x1
data_orm <- data.frame(y, x1)

## Original function to interface
orm(y ~ x1, data_orm)

## The interface puts data as first parameter
ntbt_orm(data_orm, y ~ x1)

## so it can be used easily in a pipeline.
data_orm \%>\%
  ntbt_orm(y ~ x1)


## ntbt_psm: Parametric Survival Model
n <- 400
set.seed(1)
age <- rnorm(n, 50, 12)
sex <- factor(sample(c('Female','Male'),n,TRUE))
dd <- datadist(age,sex)
options(datadist='dd')
# Population hazard function:
h <- .02*exp(.06*(age-50)+.8*(sex=='Female'))
d.time <- -log(runif(n))/h
cens <- 15*runif(n)
death <- ifelse(d.time <= cens,1,0)
d.time <- pmin(d.time, cens)

data_psm <- data.frame(d.time, death, sex, age)

## Original function to interface
psm(Surv(d.time, death) ~ sex * pol(age, 2), data_psm, dist = 'lognormal')
# Log-normal model is a bad fit for proportional hazards data

## The interface puts data as first parameter
ntbt_psm(data_psm, Surv(d.time, death) ~ sex * pol(age, 2), dist = 'lognormal')

## so it can be used easily in a pipeline.
data_psm \%>\%
  ntbt_psm(Surv(d.time, death) ~ sex * pol(age, 2), dist = 'lognormal')
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{rms}
\keyword{ntbt_bj}
\keyword{ntbt_cph}
\keyword{ntbt_Glm}
\keyword{ntbt_lrm}
\keyword{ntbt_npsurv}
\keyword{ntbt_ols}
\keyword{ntbt_orm}
\keyword{ntbt_psm}
