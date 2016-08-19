\name{partykit}
%\alias{ntbt_cforest}
%\alias{ntbt_ctree}
\alias{ntbt_glmtree}
\alias{ntbt_lmtree}
%\alias{ntbt_mob}
\alias{ntbt_palmtree}
\title{Interfaces for partykit package for data science pipelines.}
\description{
  Interfaces to \code{partykit} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
# Commented functions already defined for package party
# ntbt_cforest(data, ...)
# ntbt_ctree(data, ...)
ntbt_glmtree(data, ...)
ntbt_lmtree(data, ...)
# ntbt_mob(data, ...)
ntbt_palmtree(data, ...)
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
library(partykit)


## ntbt_cforest: Conditional Random Forests

## Original function to interface
cf <- cforest(dist ~ speed, data = cars)

## The interface puts data as first parameter
cf <- ntbt_cforest(cars, dist ~ speed)

## so it can be used easily in a pipeline.
cf <- cars \%>\%
  ntbt_cforest(dist ~ speed)


## ntbt_ctree: Conditional Inference Trees
airq <- subset(airquality, !is.na(Ozone))

## Original function to interface
airct <- ctree(Ozone ~ ., data = airq)
plot(airct)

## The interface puts data as first parameter
airct <- ntbt_ctree(airq, Ozone ~ .)
plot(airct)

## so it can be used easily in a pipeline.
airq \%>\%
  ntbt_ctree(Ozone ~ .) \%>\%
  plot()


## ntbt_glmtree: Generalized Linear Model Trees
data("PimaIndiansDiabetes", package = "mlbench")

## Original function to interface
pid_tree2 <- glmtree(diabetes ~ glucose | pregnant +
                       pressure + triceps + insulin + mass + pedigree + age,
                     data = PimaIndiansDiabetes, family = binomial)
plot(pid_tree2)

## The interface puts data as first parameter
pid_tree2 <- ntbt_glmtree(PimaIndiansDiabetes, diabetes ~ glucose | pregnant +
                            pressure + triceps + insulin + mass + pedigree + age,
                          family = binomial)
plot(pid_tree2)

## so it can be used easily in a pipeline.
PimaIndiansDiabetes \%>\%
  ntbt_glmtree(diabetes ~ glucose | pregnant +
                 pressure + triceps + insulin + mass + pedigree + age,
               family = binomial) \%>\%
  plot()


## ntbt_lmtree: Linear Model Trees
data("BostonHousing", package = "mlbench")
BostonHousing <- 
  transform(BostonHousing,
            chas = factor(chas, levels = 0:1, labels = c("no", "yes")),
            rad = factor(rad, ordered = TRUE))

## Original function to interface
bh_tree <- lmtree(medv ~ log(lstat) + I(rm^2) | zn + indus + chas +
                    nox + age + dis + rad + tax + crim + b + ptratio,
                  data = BostonHousing, minsize = 40)
plot(bh_tree)

## The interface puts data as first parameter
bh_tree <- ntbt_lmtree(BostonHousing,
                       medv ~ log(lstat) + I(rm^2) | zn + indus + chas +
                         nox + age + dis + rad + tax + crim + b + ptratio,
                       minsize = 40)
plot(bh_tree)

## so it can be used easily in a pipeline.
BostonHousing \%>\%
  ntbt_lmtree(medv ~ log(lstat) + I(rm^2) | zn + indus + chas +
                nox + age + dis + rad + tax + crim + b + ptratio,
              minsize = 40) \%>\%
  plot()


## ntbt_mob: Model-based Recursive Partitioning
data("PimaIndiansDiabetes", package = "mlbench")

logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  glm(y ~ 0 + x, family = binomial, start = start, ...)
}

## Original function to interface
pid_tree <- mob(diabetes ~ glucose | pregnant + pressure + triceps + insulin +
                  mass + pedigree + age, data = PimaIndiansDiabetes, fit = logit)
plot(pid_tree)

## The interface puts data as first parameter
pid_tree <- ntbt_mob(PimaIndiansDiabetes, diabetes ~ glucose | pregnant + pressure +
                       triceps + insulin + mass + pedigree + age, fit = logit)
plot(pid_tree)

## so it can be used easily in a pipeline.
PimaIndiansDiabetes \%>\%
  ntbt_mob(diabetes ~ glucose | pregnant + pressure +
             triceps + insulin + mass + pedigree + age, fit = logit) \%>\%
  plot()


## ntbt_palmtree: Partially Additive (Generalized) Linear Model Trees
dgp <- function(nobs = 1000, nreg = 5, creg = 0.4, ptreat = 0.5, sd = 1,
  coef = c(1, 0.25, 0.25, 0, 0, -0.25), eff = 1)
{
  d <- mvtnorm::rmvnorm(nobs,
    mean = rep(0, nreg),
    sigma = diag(1 - creg, nreg) + creg)
  colnames(d) <- paste0("x", 1:nreg)
  d <- as.data.frame(d)
  d$a <- rbinom(nobs, size = 1, prob = ptreat)
  d$err <- rnorm(nobs, mean = 0, sd = sd)

  gopt <- function(d) {
    as.numeric(d$x1 > -0.545) * as.numeric(d$x2 < 0.545)
  }
  d$y <- coef[1] + drop(as.matrix(d[, paste0("x", 1:5)]) \%*\% coef[-1]) -
    eff * (d$a - gopt(d))^2 + d$err
  d$a <- factor(d$a)
  return(d)
}
set.seed(1)
d <- dgp()

## Original function to interface
palm <- palmtree(y ~ a | x1 + x2 + x5 | x1 + x2 + x3 + x4 + x5, data = d)
plot(palm)

## The interface puts data as first parameter
palm <- ntbt_palmtree(d, y ~ a | x1 + x2 + x5 | x1 + x2 + x3 + x4 + x5)
plot(palm)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_palmtree(y ~ a | x1 + x2 + x5 | x1 + x2 + x3 + x4 + x5) \%>\%
  plot()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{partykit}
\keyword{cforest}
\keyword{ctree}
\keyword{glmtree}
\keyword{lmtree}
\keyword{mob}
\keyword{palmtree}
