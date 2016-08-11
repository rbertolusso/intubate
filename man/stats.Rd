\name{stats}
\alias{ntbt_aggregate}
\alias{ntbt_alias}
\alias{ntbt_ansari.test}
\alias{ntbt_aov}
\alias{ntbt_bartlett.test}
\alias{ntbt_cor.test}
\alias{ntbt_fligner.test}
\alias{ntbt_friedman.test}
\alias{ntbt_ftable}
\alias{ntbt_getInitial}
\alias{ntbt_glm}
\alias{ntbt_kruskal.test}
\alias{ntbt_lm}
\alias{ntbt_loess}
\alias{ntbt_lqs}
\alias{ntbt_model.frame}
\alias{ntbt_model.matrix}
\alias{ntbt_mood.test}
\alias{ntbt_nls}
\alias{ntbt_oneway.test}
\alias{ntbt_ppr}
\alias{ntbt_prcomp}
\alias{ntbt_princomp}
\alias{ntbt_quade.test}
\alias{ntbt_replications}
\alias{ntbt_t.test}
\alias{ntbt_var.test}
\alias{ntbt_wilcox.test}
\alias{ntbt_xtabs}
\title{Interfaces for stats package for data science pipelines.}
\description{
  Interfaces to \code{stats} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_aggregate(data, ...)
ntbt_alias(data, ...)
ntbt_ansari.test(data, ...)
ntbt_aov(data, ...)
ntbt_bartlett.test(data, ...)
ntbt_cor.test(data, ...)
ntbt_fligner.test(data, ...)
ntbt_friedman.test(data, ...)
ntbt_ftable(data, ...)
ntbt_getInitial(data, ...)
ntbt_glm(data, ...)
ntbt_kruskal.test(data, ...)
ntbt_lm(data, ...)
ntbt_loess(data, ...)
ntbt_lqs(data, ...)
ntbt_model.frame(data, ...)
ntbt_model.matrix(data, ...)
ntbt_mood.test(data, ...)
ntbt_nls(data, ...)
ntbt_oneway.test(data, ...)
ntbt_ppr(data, ...)
ntbt_prcomp(data, ...)
ntbt_princomp(data, ...)
ntbt_quade.test(data, ...)
ntbt_replications(data, ...)
ntbt_t.test(data, ...)
ntbt_var.test(data, ...)
ntbt_wilcox.test(data, ...)
ntbt_xtabs(data, ...)
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

## aggregate
## Original function to interface
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)

## The interface reverses the order of data and formula
ag <- ntbt_aggregate(ToothGrowth, len ~ ., mean)
ntbt_xtabs(ag, len ~ .)

## so it can be used easily in a pipeline.
ToothGrowth \%>\%
  ntbt_aggregate(len ~ ., mean) \%>\%
  ntbt_xtabs(len ~ .)
  
esoph \%>\%
  ntbt_aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, sum)

## alias
## Original function to interface
alias(yield ~ block + N*P*K, npk)

## The interface reverses the order of data and formula
ntbt_alias(npk, yield ~ block + N*P*K)

## so it can be used easily in a pipeline.
npk \%>\%
  ntbt_alias(yield ~ block + N*P*K)

## ansari.test
## Original function to interface
ansari.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_ansari.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_ansari.test(extra ~ group)

## aov
## Original function to interface
aov(yield ~ block + N * P + K, npk)

## The interface reverses the order of data and formula
ntbt_aov(npk, yield ~ block + N * P + K)

## so it can be used easily in a pipeline.
npk \%>\%
  ntbt_aov(yield ~ block + N * P + K)

## bartlett.test
## Original function to interface
bartlett.test(count ~ spray, data = InsectSprays)

## The interface reverses the order of data and formula
ntbt_bartlett.test(data = InsectSprays, count ~ spray)

## so it can be used easily in a pipeline.
InsectSprays \%>\%
  ntbt_bartlett.test(count ~ spray)

## cor.test
## Original function to interface
cor.test(~ CONT + INTG, data = USJudgeRatings)

## The interface reverses the order of data and formula
ntbt_cor.test(data = USJudgeRatings, ~ CONT + INTG)

## so it can be used easily in a pipeline.
USJudgeRatings \%>\%
  ntbt_cor.test(~ CONT + INTG)

## fligner.test
## Original function to interface
fligner.test(count ~ spray, data = InsectSprays)

## The interface reverses the order of data and formula
ntbt_fligner.test(data = InsectSprays, count ~ spray)

## so it can be used easily in a pipeline.
InsectSprays \%>\%
  ntbt_fligner.test(count ~ spray)

## friedman.test
wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)

## Original function to interface
friedman.test(x ~ w | t, data = wb)

## The interface reverses the order of data and formula
ntbt_friedman.test(data = wb, x ~ w | t)

## so it can be used easily in a pipeline.
wb \%>\%
  ntbt_friedman.test(x ~ w | t)

## ftable
## Original function to interface
x <- ftable(Survived ~ ., data = Titanic)
ftable(Sex ~ Class + Age, data = x)

## The interface reverses the order of data and formula
x <- ntbt_ftable(data = Titanic, Survived ~ .)
ftable(data = x, Sex ~ Class + Age)

## so it can be used easily in a pipeline.
Titanic \%>\%
  ntbt_ftable(Survived ~ .)

Titanic \%>\%
  ntbt_ftable(Survived ~ .) \%>\%
  ntbt_ftable(Sex ~ Class + Age)

## getInitial
PurTrt <- Puromycin[ Puromycin$state == "treated", ]

## Original function to interface
getInitial(rate ~ SSmicmen( conc, Vm, K ), PurTrt)

## The interface reverses the order of data and formula
ntbt_getInitial(PurTrt, rate ~ SSmicmen( conc, Vm, K ))

## so it can be used easily in a pipeline.
PurTrt \%>\%
  ntbt_getInitial(rate ~ SSmicmen( conc, Vm, K ))

## glm
utils::data(anorexia, package = "MASS")

## Original function to interface
anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                data = anorexia)
summary(anorex.1)

## The interface reverses the order of data and formula
anorex.1 <- ntbt_glm(data = anorexia,
                     formula = Postwt ~ Prewt + Treat + offset(Prewt))
summary(anorex.1)

## so it can be used easily in a pipeline.
anorexia \%>\%
  ntbt_glm(Postwt ~ Prewt + Treat + offset(Prewt)) \%>\%
  summary()

# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
data.frame(u = c(5,10,15,20,30,40,60,80,100),
           lot1 = c(118,58,42,35,27,25,21,19,18)
           ) \%>\%
  ntbt_glm(lot1 ~ log(u), family = Gamma) \%>\%
  summary()

## kruskal.test
## Original function to interface
kruskal.test(Ozone ~ Month, airquality)

## The interface reverses the order of data and formula
ntbt_kruskal.test(airquality, Ozone ~ Month)

## so it can be used easily in a pipeline.
airquality \%>\%
  ntbt_kruskal.test(Ozone ~ Month)

## lm
## Original function to interface
lm(conc ~ uptake, CO2)

## The interface reverses the order of data and formula
ntbt_lm(CO2, conc ~ uptake)

## so it can be used easily in a pipeline.
library(magrittr)
CO2 \%>\%
  ntbt_lm(conc ~ uptake)

CO2 \%>\%
  ntbt_lm(conc ~ uptake) \%>\%
  summary()

## loess
## Original function to interface
loess(dist ~ speed, cars)

## The interface reverses the order of data and formula
ntbt_loess(cars, dist ~ speed)

## so it can be used easily in a pipeline.
cars \%>\%
  ntbt_loess(dist ~ speed)
  
cars \%>\%
  ntbt_loess(dist ~ speed,
             control = loess.control(surface = "direct"))

## lqs
library(MASS)

## Original function to interface
set.seed(123) # make reproducible
lqs(stack.loss ~ ., data = stackloss)

## The interface reverses the order of data and formula
set.seed(123) # make reproducible
ntbt_lqs(data = stackloss, stack.loss ~ .)

## so it can be used easily in a pipeline.
set.seed(123) # make reproducible
stackloss \%>\%
  ntbt_lqs(stack.loss ~ .)

## model.frame
## Original function to interface
model.frame(dist ~ speed, data = cars)

## The interface reverses the order of data and formula
ntbt_model.frame(data = cars, dist ~ speed)

## so it can be used easily in a pipeline.
cars \%>\%
  ntbt_model.frame(dist ~ speed)

## model.matrix
dd <- data.frame(a = gl(3, 4),
                 b = gl(4, 1, 12)) # balanced 2-way

## Original function to interface
model.matrix(~ a + b, dd)

## The interface reverses the order of data and formula
ntbt_model.matrix(dd, ~ a + b)

## so it can be used easily in a pipeline.
dd \%>\%
  ntbt_model.matrix(~ a + b)

## mood.test
## Original function to interface
mood.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_mood.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
sleep \%>\%
  ntbt_mood.test(extra ~ group)

## nls
## Original function to interface
nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase)

## The interface reverses the order of data and formula
ntbt_nls(data = DNase, density ~ SSlogis(log(conc), Asym, xmid, scal))

## so it can be used easily in a pipeline.
DNase \%>\%
  ntbt_nls(density ~ SSlogis(log(conc), Asym, xmid, scal))

## oneway.test
## Original function to interface
oneway.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_oneway.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
sleep \%>\%
  ntbt_oneway.test(extra ~ group)

## ppr
## Original function to interface
ppr(log(perm) ~ area + peri + shape, data = rock,
    nterms = 2, max.terms = 5)

## The interface reverses the order of data and formula
ntbt_ppr(data = rock, log(perm) ~ area + peri + shape,
         nterms = 2, max.terms = 5)

## so it can be used easily in a pipeline.
rock \%>\%
  ntbt_ppr(log(perm) ~ area + peri + shape,
           nterms = 2, max.terms = 5)

## prcomp
## Original function to interface
prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)

## The interface reverses the order of data and formula
ntbt_prcomp(data = USArrests, ~ Murder + Assault + Rape, scale = TRUE)

## so it can be used easily in a pipeline.
USArrests \%>\%
  ntbt_prcomp(~ Murder + Assault + Rape, scale = TRUE)

## princomp
## Original function to interface
princomp(~ ., data = USArrests, cor = TRUE)

## The interface reverses the order of data and formula
ntbt_princomp(data = USArrests, ~ ., cor = TRUE)

## so it can be used easily in a pipeline.
USArrests \%>\%
  ntbt_princomp(~ ., cor = TRUE)

## quade.test
wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)

## Original function to interface
quade.test(x ~ w | t, data = wb)

## The interface reverses the order of data and formula
ntbt_quade.test(data = wb, x ~ w | t)

## so it can be used easily in a pipeline.
wb \%>\%
  ntbt_quade.test(x ~ w | t)

## replications
## From Venables and Ripley (2002) p.165.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
           55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)

## Original function to interface
replications(~ . - yield, npk)

## The interface reverses the order of data and formula
ntbt_replications(npk, ~ . - yield)

## so it can be used easily in a pipeline.
npk \%>\%
  ntbt_replications(~ . - yield)

## t.test
## Original function to interface
t.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_t.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
sleep \%>\%
  ntbt_t.test(extra ~ group)

## var.test
## Original function to interface
var.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_var.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
sleep \%>\%
  ntbt_var.test(extra ~ group)

## wilcox.test
## Original function to interface
wilcox.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_wilcox.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
sleep \%>\%
  ntbt_wilcox.test(extra ~ group)

## xtabs
## Original function to interface
ag <- aggregate(len ~ ., data = ToothGrowth, mean)
xtabs(len ~ ., data = ag)

## The interface reverses the order of data and formula
ag <- ntbt_aggregate(ToothGrowth, len ~ ., mean)
ntbt_xtabs(ag, len ~ .)

## so it can be used easily in a pipeline.
ToothGrowth \%>\%
  ntbt_aggregate(len ~ ., mean) \%>\%
  ntbt_xtabs(len ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{stats}
\keyword{aggregate}
\keyword{alias}
\keyword{ansari.test}
\keyword{aov}
\keyword{bartlett.test}
\keyword{cor.test}
\keyword{fligner.test}
\keyword{friedman.test}
\keyword{ftable}
\keyword{getInitial}
\keyword{glm}
\keyword{kruskal.test}
\keyword{lm}
\keyword{loess}
\keyword{lqs}
\keyword{model.frame}
\keyword{model.matrix}
\keyword{mood.test}
\keyword{nls}
\keyword{oneway.test}
\keyword{ppr}
\keyword{prcomp}
\keyword{princomp}
\keyword{quade.test}
\keyword{replications}
\keyword{t.test}
\keyword{var.test}
\keyword{wilcox.test}
\keyword{xtabs}
