\name{coin}
\alias{ntbt_ansari_test}
\alias{ntbt_chisq_test}
\alias{ntbt_cmh_test}
\alias{ntbt_conover_test}
\alias{ntbt_fisyat_test}
\alias{ntbt_fligner_test}
\alias{ntbt_friedman_test}
\alias{ntbt_independence_test}
\alias{ntbt_klotz_test}
\alias{ntbt_koziol_test}
\alias{ntbt_kruskal_test}
\alias{ntbt_lbl_test}
\alias{ntbt_logrank_test}
\alias{ntbt_maxstat_test}
\alias{ntbt_median_test}
\alias{ntbt_mh_test}
\alias{ntbt_mood_test}
\alias{ntbt_normal_test}
\alias{ntbt_oneway_test}
\alias{ntbt_quade_test}
\alias{ntbt_quadrant_test}
\alias{ntbt_sign_test}
\alias{ntbt_symmetry_test}
\alias{ntbt_taha_test}
\alias{ntbt_savage_test}
\alias{ntbt_spearman_test}
\alias{ntbt_wilcox_test}
\alias{ntbt_wilcoxsign_test}
\title{Interfaces for coin package for data science pipelines.}
\description{
  Interfaces to \code{coin} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_ansari_test(data, ...)
ntbt_chisq_test(data, ...)
ntbt_cmh_test(data, ...)
ntbt_conover_test(data, ...)
ntbt_fisyat_test(data, ...)
ntbt_fligner_test(data, ...)
ntbt_friedman_test(data, ...)
ntbt_independence_test(data, ...)
ntbt_klotz_test(data, ...)
ntbt_koziol_test(data, ...)
ntbt_kruskal_test(data, ...)
ntbt_lbl_test(data, ...)
ntbt_logrank_test(data, ...)
ntbt_maxstat_test(data, ...)
ntbt_median_test(data, ...)
ntbt_mh_test(data, ...)
ntbt_mood_test(data, ...)
ntbt_normal_test(data, ...)
ntbt_oneway_test(data, ...)
ntbt_quade_test(data, ...)
ntbt_quadrant_test(data, ...)
ntbt_sign_test(data, ...)
ntbt_symmetry_test(data, ...)
ntbt_taha_test(data, ...)
ntbt_savage_test(data, ...)
ntbt_spearman_test(data, ...)
ntbt_wilcox_test(data, ...)
ntbt_wilcoxsign_test(data, ...)
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
library(coin)

## Tests of Independence in Two- or Three-Way Contingency Tables
## Please contribute better example
## Original function to interface
chisq_test(Plant ~ Type, data = CO2)
cmh_test(Plant ~ Type, data = CO2)
lbl_test(Plant ~ Type, data = CO2)

## The interface puts data as first parameter
ntbt_chisq_test(CO2, Plant ~ Type)
ntbt_cmh_test(CO2, Plant ~ Type)
ntbt_lbl_test(CO2, Plant ~ Type)

## so it can be used easily in a pipeline.
CO2 \%>\%
  ntbt_chisq_test(Plant ~ Type)
CO2 \%>\%
  ntbt_cmh_test(Plant ~ Type)
CO2 \%>\%
  ntbt_lbl_test(Plant ~ Type)


## Correlation Tests
## Original function to interface
## Asymptotic Spearman test
spearman_test(CONT ~ INTG, data = USJudgeRatings)
## Asymptotic Fisher-Yates test
fisyat_test(CONT ~ INTG, data = USJudgeRatings)
## Asymptotic quadrant test
quadrant_test(CONT ~ INTG, data = USJudgeRatings)
## Asymptotic Koziol-Nemec test
koziol_test(CONT ~ INTG, data = USJudgeRatings)

## The interface puts data as first parameter
## Asymptotic Spearman test
ntbt_spearman_test(USJudgeRatings, CONT ~ INTG)
## Asymptotic Fisher-Yates test
ntbt_fisyat_test(USJudgeRatings, CONT ~ INTG)
## Asymptotic quadrant test
ntbt_quadrant_test(USJudgeRatings, CONT ~ INTG)
## Asymptotic Koziol-Nemec test
ntbt_koziol_test(USJudgeRatings, CONT ~ INTG)

## so it can be used easily in a pipeline.
## Asymptotic Spearman test
USJudgeRatings \%>\%
  ntbt_spearman_test(CONT ~ INTG)
## Asymptotic Fisher-Yates test
USJudgeRatings \%>\%
  ntbt_fisyat_test(CONT ~ INTG)
## Asymptotic quadrant test
USJudgeRatings \%>\%
  ntbt_quadrant_test(CONT ~ INTG)
## Asymptotic Koziol-Nemec test
USJudgeRatings \%>\%
  ntbt_koziol_test(CONT ~ INTG)

## ntbt_independence_test: General Independence Test
## Original function to interface
independence_test(asat ~ group, data = asat, distribution = "exact",
                  alternative = "greater", 
                  ytrafo = function(data)
                    trafo(data, numeric_trafo = normal_trafo),
                  xtrafo = function(data)
                      trafo(data, factor_trafo = function(x)
                          matrix(x == levels(x)[1], ncol = 1)))

## The interface puts data as first parameter
ntbt_independence_test(asat, asat ~ group, distribution = "exact",
                       alternative = "greater", 
                       ytrafo = function(data)
                         trafo(data, numeric_trafo = normal_trafo),
                       xtrafo = function(data)
                         trafo(data, factor_trafo = function(x)
                           matrix(x == levels(x)[1], ncol = 1)))

## so it can be used easily in a pipeline.
asat \%>\%
  ntbt_independence_test(asat ~ group, distribution = "exact",
                         alternative = "greater", 
                         ytrafo = function(data)
                           trafo(data, numeric_trafo = normal_trafo),
                         xtrafo = function(data)
                           trafo(data, factor_trafo = function(x)
                             matrix(x == levels(x)[1], ncol = 1)))


## Two- and K-Sample Location Tests
## Tritiated Water Diffusion Across Human Chorioamnion
## Hollander and Wolfe (1999, p. 110, Tab. 4.1)
diffusion <- data.frame(
    pd = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46,
           1.15, 0.88, 0.90, 0.74, 1.21),
    age = factor(rep(c("At term", "12-26 Weeks"), c(10, 5)))
)
ex <- data.frame(
    y = c(3, 4, 8, 9, 1, 2, 5, 6, 7),
    x = factor(rep(c("no", "yes"), c(4, 5)))
)

## Original function to interface
kruskal_test(pd ~ age, data = diffusion, distribution = "exact")
median_test(y ~ x, data = ex, distribution = "exact")
normal_test(pd ~ age, data = diffusion, distribution = "exact", conf.int = TRUE)
oneway_test(pd ~ age, data = diffusion)
savage_test(pd ~ age, data = diffusion, distribution = "exact", conf.int = TRUE)
wilcox_test(pd ~ age, data = diffusion, distribution = "exact", conf.int = TRUE)

## The interface puts data as first parameter
ntbt_kruskal_test(diffusion, pd ~ age, distribution = "exact")
ntbt_median_test(ex, y ~ x, distribution = "exact")
ntbt_normal_test(diffusion, pd ~ age, distribution = "exact", conf.int = TRUE)
ntbt_oneway_test(diffusion, pd ~ age)
ntbt_savage_test(diffusion, pd ~ age, distribution = "exact", conf.int = TRUE)
ntbt_wilcox_test(diffusion, pd ~ age, distribution = "exact", conf.int = TRUE)

## so it can be used easily in a pipeline.
diffusion \%>\%
  ntbt_kruskal_test(pd ~ age, distribution = "exact")
ex \%>\%
  ntbt_median_test(y ~ x, distribution = "exact")
diffusion \%>\%
  ntbt_normal_test(pd ~ age, distribution = "exact", conf.int = TRUE)
diffusion \%>\%
  ntbt_oneway_test(pd ~ age)
diffusion \%>\%
  ntbt_savage_test(pd ~ age, distribution = "exact", conf.int = TRUE)
diffusion \%>\%
  ntbt_wilcox_test(pd ~ age, distribution = "exact", conf.int = TRUE)

performance <- matrix(
    c(794, 150,
       86, 570),
    nrow = 2, byrow = TRUE,
    dimnames = list(
         "First" = c("Approve", "Disprove"),
        "Second" = c("Approve", "Disprove")
    )
)

## ntbt_mh_test: Marginal Homogeneity Tests
## Effectiveness of different media for the growth of diphtheria
## Cochran (1950, Tab. 2)
cases <- c(4, 2, 3, 1, 59)
n <- sum(cases)
cochran <- data.frame(
    diphtheria = factor(
        unlist(rep(list(c(1, 1, 1, 1),
                        c(1, 1, 0, 1),
                        c(0, 1, 1, 1),
                        c(0, 1, 0, 1),
                        c(0, 0, 0, 0)),
                   cases))
    ),
    media = factor(rep(LETTERS[1:4], n)),
    case =  factor(rep(seq_len(n), each = 4))
)

## Original function to interface
mh_test(diphtheria ~ media | case, data = cochran)

## The interface puts data as first parameter
ntbt_mh_test(cochran, diphtheria ~ media | case)

## so it can be used easily in a pipeline.
cochran \%>\%
  ntbt_mh_test(diphtheria ~ media | case)

## ntbt_maxstat_test: Generalized Maximally Selected Statistics
## Original function to interface
maxstat_test(counts ~ coverstorey, data = treepipit)

## The interface puts data as first parameter
ntbt_maxstat_test(treepipit, counts ~ coverstorey)

## so it can be used easily in a pipeline.
treepipit \%>\%
  ntbt_maxstat_test(counts ~ coverstorey)


## Two- and K-Sample Scale Tests
## Serum Iron Determination Using Hyland Control Sera
## Hollander and Wolfe (1999, p. 147, Tab 5.1)
sid <- data.frame(
    serum = c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
              101, 96, 97, 102, 107, 113, 116, 113, 110, 98,
              107, 108, 106, 98, 105, 103, 110, 105, 104,
              100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99),
    method = gl(2, 20, labels = c("Ramsay", "Jung-Parekh"))
)

## Original function to interface
ansari_test(serum ~ method, data = sid)
conover_test(serum ~ method, data = sid)
fligner_test(serum ~ method, data = sid)
klotz_test(serum ~ method, data = sid)
mood_test(serum ~ method, data = sid)
taha_test(serum ~ method, data = sid)

## The interface puts data as first parameter
ntbt_ansari_test(sid, serum ~ method)
ntbt_conover_test(sid, serum ~ method)
ntbt_fligner_test(sid, serum ~ method)
ntbt_klotz_test(sid, serum ~ method)
ntbt_mood_test(sid, serum ~ method)
ntbt_taha_test(sid, serum ~ method)

## so it can be used easily in a pipeline.
sid \%>\%
  ntbt_ansari_test(serum ~ method)
sid \%>\%
  ntbt_conover_test(serum ~ method)
sid \%>\%
  ntbt_fligner_test(serum ~ method)
sid \%>\%
  ntbt_klotz_test(serum ~ method)
sid \%>\%
  ntbt_mood_test(serum ~ method)
sid \%>\%
  ntbt_taha_test(serum ~ method)

## ntbt_logrank_test: Two- and K-Sample Tests for Censored Data
## Example data (Callaert, 2003, Tab.1)
callaert <- data.frame(
    time = c(1, 1, 5, 6, 6, 6, 6, 2, 2, 2, 3, 4, 4, 5, 5),
    group = factor(rep(0:1, c(7, 8)))
)
## Original function to interface
logrank_test(Surv(time) ~ group, data = callaert, distribution = "exact")

## The interface puts data as first parameter
ntbt_logrank_test(callaert, Surv(time) ~ group, distribution = "exact")

## so it can be used easily in a pipeline.
callaert \%>\%
  ntbt_logrank_test(Surv(time) ~ group, distribution = "exact")


## ntbt_symmetry_test: General Symmetry Test
## One-sided exact Fisher-Pitman test for paired observations
y1 <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y2 <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
dta <- data.frame(
    y = c(y1, y2),
    x = gl(2, length(y1)),
    block = factor(rep(seq_along(y1), 2))
)

## Original function to interface
symmetry_test(y ~ x | block, data = dta, distribution = "exact", alternative = "greater")

## The interface puts data as first parameter
ntbt_symmetry_test(dta, y ~ x | block, distribution = "exact", alternative = "greater")

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_symmetry_test(y ~ x | block, distribution = "exact", alternative = "greater")


## Symmetry Tests
## Data with explicit group and block information
dta <- data.frame(y = c(y1, y2), x = gl(2, length(y1)),
                  block = factor(rep(seq_along(y1), 2)))

## Original function to interface
## For two samples, the sign test is equivalent to the Friedman test...
sign_test(y ~ x | block, data = dta, distribution = "exact")
friedman_test(y ~ x | block, data = dta, distribution = "exact")
## ...and the signed-rank test is equivalent to the Quade test
wilcoxsign_test(y ~ x | block, data = dta, distribution = "exact")
quade_test(y ~ x | block, data = dta, distribution = "exact")

## The interface puts data as first parameter
ntbt_sign_test(dta, y ~ x | block, distribution = "exact")
ntbt_friedman_test(dta, y ~ x | block, distribution = "exact")
ntbt_wilcoxsign_test(dta, y ~ x | block, distribution = "exact")
ntbt_quade_test(dta, y ~ x | block, distribution = "exact")

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_sign_test(y ~ x | block, distribution = "exact")
dta \%>\%
  ntbt_friedman_test(y ~ x | block, distribution = "exact")
dta \%>\%
  ntbt_wilcoxsign_test(y ~ x | block, distribution = "exact")
dta \%>\%
  ntbt_quade_test(y ~ x | block, distribution = "exact")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{coin}
\keyword{ansari_test}
\keyword{chisq_test}
\keyword{cmh_test}
\keyword{conover_test}
\keyword{fisyat_test}
\keyword{fligner_test}
\keyword{friedman_test}
\keyword{independence_test}
\keyword{klotz_test}
\keyword{koziol_test}
\keyword{kruskal_test}
\keyword{lbl_test}
\keyword{logrank_test}
\keyword{maxstat_test}
\keyword{median_test}
\keyword{mh_test}
\keyword{mood_test}
\keyword{normal_test}
\keyword{oneway_test}
\keyword{quade_test}
\keyword{quadrant_test}
\keyword{sign_test}
\keyword{symmetry_test}
\keyword{taha_test}
\keyword{savage_test}
\keyword{spearman_test}
\keyword{wilcox_test}
\keyword{wilcoxsign_test}
