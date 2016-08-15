\name{lme4}
\alias{ntbt_glmer}
\alias{ntbt_glmer.nb}
\alias{ntbt_glFormula}
\alias{ntbt_lFormula}
\alias{ntbt_lmer}
%\alias{ntbt_lmList}
\alias{ntbt_nlmer}
\title{Interfaces for lme4 package for data science pipelines.}
\description{
  Interfaces to \code{lme4} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_glmer(data, ...)
ntbt_glmer.nb(data, ...)
ntbt_glFormula(data, ...)
ntbt_lFormula(data, ...)
ntbt_lmer(data, ...)
# ntbt_lmList(data, ...)   ## Already defined in nlme
ntbt_nlmer(data, ...)
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
library(lme4)

## ntbt_glmer: Fitting Generalized Linear Mixed-Effects Models
## Original function to interface
glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
      data = cbpp, family = binomial)

## The interface puts data as first parameter
ntbt_glmer(cbpp, cbind(incidence, size - incidence) ~ period + (1 | herd),
           family = binomial)

## so it can be used easily in a pipeline.
cbpp \%>\%
  ntbt_glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             family = binomial)


## ntbt_glmer.nb: Fitting Negative Binomial GLMMs
set.seed(101)
dd <- expand.grid(f1 = factor(1:3),
                  f2 = LETTERS[1:2], g=1:9, rep=1:15,
                  KEEP.OUT.ATTRS=FALSE)
summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)

## Original function to interface
glmer.nb(y ~ f1*f2 + (1|g), data = dd, verbose = FALSE)
 
## The interface puts data as first parameter
ntbt_glmer.nb(dd, y ~ f1*f2 + (1|g), verbose = FALSE)

## so it can be used easily in a pipeline.
dd \%>\%
  ntbt_glmer.nb(y ~ f1*f2 + (1|g), verbose = FALSE)


## ntbt_lmer: Fit Linear Mixed-Effects Models

## Original function to interface
lmer(Reaction ~ Days + (Days || Subject), sleepstudy)

## The interface puts data as first parameter
ntbt_lmer(sleepstudy, Reaction ~ Days + (Days || Subject))

## so it can be used easily in a pipeline.
sleepstudy \%>\%
  ntbt_lmer(Reaction ~ Days + (Days || Subject))


## ntbt_lmList: Fit List of lm Objects with a Common Model
## Original function to interface
lmList(Reaction ~ Days | Subject, sleepstudy)

## The interface puts data as first parameter
ntbt_lmList(sleepstudy, Reaction ~ Days | Subject)

## so it can be used easily in a pipeline.
sleepstudy \%>\%
  ntbt_lmList(Reaction ~ Days | Subject)


## Original function to interface
lFormula(Reaction ~ Days + (Days|Subject), sleepstudy)
glFormula(Reaction ~ Days + (Days|Subject), sleepstudy)

## The interface puts data as first parameter
ntbt_lFormula(sleepstudy, Reaction ~ Days + (Days|Subject))
ntbt_glFormula(sleepstudy, Reaction ~ Days + (Days|Subject))

## so it can be used easily in a pipeline.
sleepstudy \%>\%
  ntbt_lFormula(Reaction ~ Days + (Days|Subject))
sleepstudy \%>\%
  ntbt_glFormula(Reaction ~ Days + (Days|Subject))


## ntbt_nlmer: Fitting Nonlinear Mixed-Effects Models
## Original function to interface
nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
      Orange, start = c(Asym = 200, xmid = 725, scal = 350))

## The interface puts data as first parameter
ntbt_nlmer(Orange, circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
           start = c(Asym = 200, xmid = 725, scal = 350))

## so it can be used easily in a pipeline.
Orange \%>\%
  ntbt_nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
             start = c(Asym = 200, xmid = 725, scal = 350))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{lme4}
\keyword{glmer}
\keyword{glmer.nb}
\keyword{glFormula}
\keyword{lFormula}
\keyword{lmer}
\keyword{lmList}
\keyword{nlmer}
