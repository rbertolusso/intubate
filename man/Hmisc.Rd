\name{Hmisc}
\alias{ntbt_areg.boot}
\alias{ntbt_aregImpute}
\alias{ntbt_biVar}
\alias{ntbt_bpplotM}
\alias{ntbt_dataRep}
\alias{ntbt_describe}
\alias{ntbt_Dotplot}
\alias{ntbt_Ecdf}
\alias{ntbt_fit.mult.impute}
\alias{ntbt_nobsY}
\alias{ntbt_rcorrcens}
\alias{ntbt_redun}
\alias{ntbt_summary}
\alias{ntbt_summaryD}
\alias{ntbt_summaryM}
\alias{ntbt_summaryP}
\alias{ntbt_summaryRc}
\alias{ntbt_summaryS}
\alias{ntbt_transcan}
\alias{ntbt_varclus}
\alias{ntbt_xYplot}
\title{Interfaces for Hmisc package for data science pipelines.}
\description{
  Interfaces to \code{Hmisc} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_areg.boot(data, ...)
ntbt_aregImpute(data, ...)
ntbt_biVar(data, ...)
ntbt_bpplotM(data, ...)
ntbt_dataRep(data, ...)
ntbt_describe(data, ...)
ntbt_Dotplot(data, ...)
ntbt_Ecdf(data, ...)
ntbt_fit.mult.impute(data, ...)
ntbt_nobsY(data, ...)
ntbt_rcorrcens(data, ...)
ntbt_redun(data, ...)
ntbt_summary(data, ...)
ntbt_summaryD(data, ...)
ntbt_summaryM(data, ...)
ntbt_summaryP(data, ...)
ntbt_summaryRc(data, ...)
ntbt_summaryS(data, ...)
ntbt_transcan(data, ...)
ntbt_varclus(data, ...)
ntbt_xYplot(data, ...)
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
library(Hmisc)

## ntbt_areg.boot
set.seed(171)  # to be able to reproduce example
x1 <- rnorm(200)
x2 <- runif(200)  # a variable that is really unrelated to y]
x3 <- factor(sample(c('cat','dog','cow'), 200,TRUE))  # also unrelated to y
y  <- exp(x1 + rnorm(200)/3)

data <- data.frame(y, x1, x2, x3)

## Original function to interface
f  <- areg.boot(y ~ x1 + x2 + x3, data, B = 40)
plot(f)

## The interface puts data as first parameter
f  <- ntbt_areg.boot(data, y ~ x1 + x2 + x3, B = 40)
plot(f)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_areg.boot(y ~ x1 + x2 + x3, B = 40) \%>\%
  plot()


## ntbt_aregImpute
x1 <- factor(sample(c('a','b','c'),1000,TRUE))
x2 <- (x1=='b') + 3*(x1=='c') + rnorm(1000,0,2)
x3 <- rnorm(1000)
y  <- x2 + 1*(x1=='c') + .2*x3 + rnorm(1000,0,2)
orig.x1 <- x1[1:250]
orig.x2 <- x2[251:350]
x1[1:250] <- NA
x2[251:350] <- NA
d <- data.frame(x1, x2, x3, y)

## Original function to interface
# Find value of nk that yields best validating imputation models
# tlinear=FALSE means to not force the target variable to be linear
f <- aregImpute(~y + x1 + x2 + x3, nk=c(0,3:5), tlinear=FALSE,
                data=d, B=10) # normally B=75
plot(f)
## The interface puts data as first parameter
f <- ntbt_aregImpute(d, ~y + x1 + x2 + x3, nk=c(0,3:5), tlinear=FALSE, B=10)
plot(f)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_aregImpute(~y + x1 + x2 + x3, nk=c(0,3:5), tlinear=FALSE, B=10) \%>\%
  plot()


## biVar
## NOTE: not seen any working example.
## I am too lazy (ignorant, really...) to produce one.
## Please contribute.

## Original function to interface
## The interface puts data as first parameter
## so it can be used easily in a pipeline.


## ntbt_bpplotM
set.seed(1)
n <- 800
d <- data.frame(treatment = sample(c('a','b'), n, TRUE),
                sex = sample(c('female','male'), n, TRUE),
                age = rnorm(n, 40, 10),
                bp = rnorm(n, 120, 12),
                wt = rnorm(n, 190, 30))
label(d$bp) <- 'Systolic Blood Pressure'
units(d$bp) <- 'mmHg'

## Original function to interface
bpplotM(age + bp + wt ~ treatment * sex, data=d, violin = TRUE,
        violin.opts = list(col = adjustcolor('blue', alpha.f = .15),
                           border = FALSE))

## The interface puts data as first parameter
ntbt_bpplotM(d, age + bp + wt ~ treatment * sex, violin = TRUE,
             violin.opts = list(col = adjustcolor('blue', alpha.f= .15),
                                border = FALSE))

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_bpplotM(age + bp + wt ~ treatment * sex, violin = TRUE,
               violin.opts = list(col = adjustcolor('blue', alpha.f= .15),
                                  border = FALSE))


## dataRep
set.seed(13)
num.symptoms <- sample(1:4, 1000,TRUE)
sex <- factor(sample(c('female','male'), 1000,TRUE))
x    <- runif(1000)
x[1] <- NA
table(num.symptoms, sex, .25*round(x/.25))
data <- data.frame(num.symptoms, sex, x)

## Original function to interface
d <- dataRep(~ num.symptoms + sex + roundN(x, .25), data)
print(d, long = TRUE)

## The interface puts data as first parameter
d <- ntbt_dataRep(data, ~ num.symptoms + sex + roundN(x, .25))
print(d, long = TRUE)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_dataRep(~ num.symptoms + sex + roundN(x, .25)) \%>\%
  print(long = TRUE)


## ntbt_describe
## Original function to interface
describe(~ conc + Type, data = CO2)

## The interface puts data as first parameter
ntbt_describe(CO2, ~ conc + Type)

## so it can be used easily in a pipeline.
CO2 \%>\%
  ntbt_describe(~ conc + Type)


## ntbt_Dotplot
set.seed(111)
dfr <- expand.grid(month=1:12, year=c(1997,1998), reps=1:100)
month <- dfr$month; year <- dfr$year
y <- abs(month-6.5) + 2*runif(length(month)) + year-1997
s <- summarize(y, llist(month,year), smedian.hilow, conf.int=.5) 

## Original function to interface
Dotplot(month ~ Cbind(y, Lower, Upper) | year, data = s) 

## The interface puts data as first parameter
ntbt_Dotplot(s, month ~ Cbind(y, Lower, Upper) | year) 

## so it can be used easily in a pipeline.
s \%>\%
  ntbt_Dotplot(month ~ Cbind(y, Lower, Upper) | year) 


## ntbt_Ecdf
set.seed(135)
m <- data.frame(ch = rnorm(1000, 200, 40),
                pre.test = rnorm(100,50,10),
                post.test = rnorm(100,55,10),
                sex = sample(c('male','female'),100,TRUE),
                region = factor(sample(c('Europe','USA','Australia'),100,TRUE)),
                year = factor(sample(2001:2002,1000,TRUE)))

## Original function to interface
Ecdf(~ ch | region * year, groups = sex, m)

## The interface puts data as first parameter
ntbt_Ecdf(m, ~ ch | region * year, groups = sex)

## so it can be used easily in a pipeline.
m \%>\%
  ntbt_Ecdf(~ ch | region * year, groups = sex)


## ntbt_nobsY
d <- expand.grid(sex=c('female', 'male', NA),
                 country=c('US', 'Romania'),
                 reps=1:2)
d$subject.id <- c(0, 0, 3:12)
dm <- addMarginal(d, sex, country)

## Original function to interface
nobsY(sex + country ~ id(subject.id) + reps, group = 'reps', data = d)

## The interface puts data as first parameter
ntbt_nobsY(d, sex + country ~ id(subject.id) + reps, group = 'reps')

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_nobsY(sex + country ~ id(subject.id) + reps, group = 'reps')


## ntbt_rcorrcens
set.seed(1)
x <- round(rnorm(200))
y <- rnorm(200)
rcorr.cens(x, y, outx=TRUE)   # can correlate non-censored variables
library(survival)
age <- rnorm(400, 50, 10)
bp  <- rnorm(400,120, 15)
bp[1]  <- NA
d.time <- rexp(400)
cens   <- runif(400,.5,2)
death  <- d.time <= cens
d.time <- pmin(d.time, cens)
data <- data.frame(d.time, death, age, bp)

## Original function to interface
r <- rcorrcens(Surv(d.time, death) ~ age + bp, data = data)
plot(r)

## The interface puts data as first parameter
r <- ntbt_rcorrcens(data, Surv(d.time, death) ~ age + bp)
plot(r)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_rcorrcens(Surv(d.time, death) ~ age + bp) \%>\%
  plot()


## ntbt_redun
set.seed(1)
n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- x1 + x2 + runif(n) / 10
x4 <- x1 + x2 + x3 + runif(n) / 10
x5 <- factor(sample(c('a','b','c'), n, replace = TRUE))
x6 <- 1 * (x5 == 'a' | x5 == 'c')
data <- data.frame(x1, x2, x3, x4, x5, x6)
  
## Original function to interface
redun(~ x1 + x2 + x3 + x4 + x5 + x6, data, r2 = .8, allcat = TRUE)

## The interface puts data as first parameter
ntbt_redun(data, ~ x1 + x2 + x3 + x4 + x5 + x6, r2 = .8, allcat = TRUE)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_redun(~ x1 + x2 + x3 + x4 + x5 + x6, r2 = .8, allcat = TRUE)


## ntbt_summary
options(digits=3)
set.seed(173)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
treatment <- factor(sample(c("Drug","Placebo"), 500, rep=TRUE))
# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache', 'Stomach Ache', 'Hangnail',
          'Muscle Ache', 'Depressed')
symptom1 <- sample(symp, 500,TRUE)
symptom2 <- sample(symp, 500,TRUE)
symptom3 <- sample(symp, 500,TRUE)
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
data <- data.frame(sex, age, treatment, Symptoms)

## Original function to interface
summary(sex ~ treatment + Symptoms, data, fun = table)
summary(age ~ sex + treatment + Symptoms, data)

## The interface puts data as first parameter
ntbt_summary(data, sex ~ treatment + Symptoms, fun = table)
ntbt_summary(data, age ~ sex + treatment + Symptoms)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_summary(sex ~ treatment + Symptoms, fun = table)
data \%>\%
  ntbt_summary(age ~ sex + treatment + Symptoms)


## ntbt_summaryD
set.seed(135)
maj <- factor(c(rep('North',13),rep('South',13)))
g <- paste('Category',rep(letters[1:13],2))
y1 <- runif(26)
data <- data.frame(maj, g, y1)

## Original function to interface
summaryD(y1 ~ maj + g, xlab='Mean', data)

## The interface puts data as first parameter
ntbt_summaryD(data, y1 ~ maj + g, xlab='Mean')

## so it can be used easily in a pipeline.
par(mfrow=c(1,1))
data \%>\%
  ntbt_summaryD(y1 ~ maj + g, xlab='Mean')


## ntbt_summaryM
options(digits=3)
set.seed(173)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
country <- factor(sample(c('US', 'Canada'), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
sbp <- rnorm(500, 120, 12)
label(sbp) <- 'Systolic BP'
units(sbp) <- 'mmHg'
treatment <- factor(sample(c("Drug","Placebo"), 500, rep=TRUE))
treatment[1]
sbp[1] <- NA

# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache','Stomach Ache','Hangnail',
          'Muscle Ache','Depressed')
symptom1 <- sample(symp, 500,TRUE)
symptom2 <- sample(symp, 500,TRUE)
symptom3 <- sample(symp, 500,TRUE)
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
data <- data.frame(age, sex, sbp, Symptoms, treatment)
# Note: In this example, some subjects have the same symptom checked
# multiple times; in practice these redundant selections would be NAs
# mChoice will ignore these redundant selections

## Original function to interface
f <- summaryM(age + sex + sbp + Symptoms ~ treatment, data = data, test = TRUE)
print(f, long = TRUE)

## The interface puts data as first parameter
f <- ntbt_summaryM(data, age + sex + sbp + Symptoms ~ treatment, test = TRUE)
print(f, long = TRUE)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_summaryM(age + sex + sbp + Symptoms ~ treatment, test = TRUE) \%>\%
  print(long = TRUE)


## ntbt_summaryP
n <- 100
f <- function(na=FALSE) {
  x <- sample(c('N', 'Y'), n, TRUE)
  if(na) x[runif(100) < .1] <- NA
  x
}
set.seed(1)
d <- data.frame(x1=f(), x2=f(), x3=f(), x4=f(), x5=f(), x6=f(), x7=f(TRUE),
                age=rnorm(n, 50, 10),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE))
d <- upData(d, labels=c(x1='MI', x2='Stroke', x3='AKI', x4='Migraines',
                 x5='Pregnant', x6='Other event', x7='MD withdrawal',
                 race='Race', sex='Sex'))

## Original function to interface
s <- summaryP(race + sex + ynbind(x1, x2, x3, x4, x5, x6, x7, label = 'Exclusions') ~
                region + treat, data=d)
plot(s, groups = 'treat')

## The interface puts data as first parameter
s <- ntbt_summaryP(d, race + sex + ynbind(x1, x2, x3, x4, x5, x6, x7, label = 'Exclusions') ~
                     region + treat)
plot(s, groups = 'treat')

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_summaryP(race + sex + ynbind(x1, x2, x3, x4, x5, x6, x7, label = 'Exclusions') ~
                  region + treat) \%>\%
  plot(groups = 'treat')


## ntbt_summaryRc
options(digits=3)
set.seed(177)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
bp  <- rnorm(500, 120, 7)
units(age) <- 'Years'; units(bp) <- 'mmHg'
label(bp) <- 'Systolic Blood Pressure'
L <- .5*(sex == 'm') + 0.1 * (age - 50)
y <- rbinom(500, 1, plogis(L))
data <- data.frame(y, age, bp, sex)
par(mfrow=c(1,2))

## Original function to interface
summaryRc(y ~ age + bp + stratify(sex), data,
          label.curves = list(keys = 'lines'), nloc = list(x = .1, y = .05))

## The interface puts data as first parameter
ntbt_summaryRc(data, y ~ age + bp + stratify(sex),
               label.curves = list(keys = 'lines'), nloc = list(x = .1, y = .05))

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_summaryRc(y ~ age + bp + stratify(sex),
                 label.curves = list(keys = 'lines'), nloc = list(x = .1, y = .05))


## ntbt_summaryS
set.seed(1)
d <- data.frame(sbp=rnorm(n, 120, 10),
                dbp=rnorm(n, 80, 10),
                age=rnorm(n, 50, 10),
                days=sample(1:n, n, TRUE),
                S1=Surv(2*runif(n)), S2=Surv(runif(n)),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE),
                meda=sample(0:1, n, TRUE), medb=sample(0:1, n, TRUE))

d <- upData(d, labels=c(sbp='Systolic BP', dbp='Diastolic BP',
            race='Race', sex='Sex', treat='Treatment',
            days='Time Since Randomization',
            S1='Hospitalization', S2='Re-Operation',
            meda='Medication A', medb='Medication B'),
            units=c(sbp='mmHg', dbp='mmHg', age='Year', days='Days'))

## Original function to interface
s <- summaryS(age + sbp + dbp ~ days + region + treat,  data = d)
plot(s, groups = 'treat', panel = panel.loess, key = list(space = 'bottom', columns = 2),
     datadensity = TRUE, scat1d.opts = list(lwd = .5))

## The interface puts data as first parameter
s <- ntbt_summaryS(d, age + sbp + dbp ~ days + region + treat)
plot(s, groups = 'treat', panel = panel.loess, key = list(space = 'bottom', columns = 2),
     datadensity = TRUE, scat1d.opts = list(lwd = .5))

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_summaryS(age + sbp + dbp ~ days + region + treat) \%>\%
  plot(groups = 'treat', panel = panel.loess, key = list(space = 'bottom', columns = 2),
       datadensity = TRUE, scat1d.opts = list(lwd = .5))


## ntbt_transcan, ntbt_fit.mult.impute
set.seed(1)
x1 <- factor(sample(c('a','b','c'),100,TRUE))
x2 <- (x1=='b') + 3*(x1=='c') + rnorm(100)
y  <- x2 + 1*(x1=='c') + rnorm(100)
x1[1:20] <- NA
x2[18:23] <- NA
d4 <- data.frame(x1,x2,y)

options(digits = 3)

## Original function to interface
f  <- transcan(~y + x1 + x2, n.impute = 10, shrink = TRUE, data = d4)
summary(f)
h <- fit.mult.impute(y ~ x1 + x2, lm, f, data = d4)
summary(h)

## The interface puts data as first parameter
f  <- ntbt_transcan(d4, ~y + x1 + x2, n.impute = 10, shrink = TRUE)
summary(f)
h <- ntbt_fit.mult.impute(d4, y ~ x1 + x2, lm, f)
summary(h)

## so it can be used easily in a pipeline.
d4 \%>\%
  ntbt_transcan(~y + x1 + x2, n.impute = 10, shrink = TRUE) \%>\%
  summary()

d4 \%>\%
  ntbt_fit.mult.impute(y ~ x1 + x2, lm, f) \%>\%
  summary()


## ntbt_varclus
set.seed(1)
x1 <- rnorm(200)
x2 <- rnorm(200)
x3 <- x1 + x2 + rnorm(200)
x4 <- x2 + rnorm(200)
data <- data.frame(x1, x2, x3, x4)

par(mfrow = c(1,1))

## Original function to interface
v <- varclus(~ x1 + x2 + x3 + x4, similarity = "spearman", data = data )
plot(v)

## The interface puts data as first parameter
v <- ntbt_varclus(data, ~ x1 + x2 + x3 + x4, similarity = "spearman")
plot(v)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_varclus(~ x1 + x2 + x3 + x4, similarity = "spearman") \%>\%
  plot()


## ntbt_xYplot
d <- expand.grid(x = seq(0, 2 * pi, length=150), p = 1:3, shift = c(0, pi)) 

## Original function to interface
xYplot(sin(x + shift)^p ~ x | shift, groups = p, data = d, type = 'l')

## The interface puts data as first parameter
ntbt_xYplot(d, sin(x + shift)^p ~ x | shift, groups = p, type = 'l')

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_xYplot(sin(x + shift)^p ~ x | shift, groups = p, type = 'l')
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{Hmisc}
\keyword{areg.boot}
\keyword{aregImpute}
\keyword{biVar}
\keyword{bpplotM}
\keyword{dataRep}
\keyword{describe}
\keyword{Dotplot}
\keyword{Ecdf}
\keyword{fit.mult.impute}
\keyword{nobsY}
\keyword{rcorrcens}
\keyword{redun}
\keyword{summary}
\keyword{summaryD}
\keyword{summaryM}
\keyword{summaryP}
\keyword{summaryRc}
\keyword{summaryS}
\keyword{transcan}
\keyword{varclus}
\keyword{xYplot}
