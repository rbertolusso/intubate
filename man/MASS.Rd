\name{MASS}
\alias{ntbt_corresp}
\alias{ntbt_glm.nb}
\alias{ntbt_lda}
\alias{ntbt_lm.gls}
\alias{ntbt_lm.ridge}
\alias{ntbt_loglm}
\alias{ntbt_logtrans}
\alias{ntbt_polr}
\alias{ntbt_qda}
\alias{ntbt_rlm}
\title{Interfaces for MASS package for data science pipelines.}
\description{
  Interfaces to \code{MASS} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_corresp(data, ...)
ntbt_glm.nb(data, ...)
ntbt_lda(data, ...)
ntbt_lm.gls(data, ...)
ntbt_lm.ridge(data, ...)
ntbt_loglm(data, ...)
ntbt_logtrans(data, ...)
ntbt_polr(data, ...)
ntbt_qda(data, ...)
ntbt_rlm(data, ...)
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
library(MASS)

## corresp
## Original function to interface
corresp(~ Age + Eth, data = quine)

## The interface reverses the order of data and formula
ntbt_corresp(data = quine, ~ Age + Eth)

## so it can be used easily in a pipeline.
quine \%>\%
  ntbt_corresp(~ Age + Eth)
  
## glm.nb
## Original function to interface
glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)

## The interface reverses the order of data and formula
ntbt_glm.nb(data = quine, Days ~ Sex/(Age + Eth*Lrn))

## so it can be used easily in a pipeline.
quine \%>\%
  ntbt_glm.nb(Days ~ Sex/(Age + Eth*Lrn))

## lda
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))

## Original function to interface
lda(Sp ~ ., Iris)

## The interface reverses the order of data and formula
ntbt_lda(Iris, Sp ~ .)

## so it can be used easily in a pipeline.
Iris \%>\%
  ntbt_lda(Sp ~ .)

stackloss \%>\%
  ntbt_lda(stack.loss ~ .) \%>\%
  summary()

## lm.gls
## Original function to interface
lm.gls(conc ~ uptake, CO2, W = diag(nrow(CO2)))

## The interface reverses the order of data and formula
ntbt_lm.gls(CO2, conc ~ uptake, W = diag(nrow(CO2)))

## so it can be used easily in a pipeline.
CO2 \%>\%
  ntbt_lm.gls(conc ~ uptake, W = diag(nrow(CO2)))

## lm.ridge
## Original function to interface
lm.ridge(GNP.deflator ~ ., longley)

## The interface reverses the order of data and formula
ntbt_lm.ridge(longley, GNP.deflator ~ .)

## so it can be used easily in a pipeline.
longley \%>\%
  ntbt_lm.ridge(GNP.deflator ~ .)

## loglm
## Original function to interface
xtCars93 <- xtabs(~ Type + Origin, Cars93)
loglm(~ Type + Origin, xtCars93)

## The interface reverses the order of data and formula
xtCars93 <- ntbt_xtabs(Cars93, ~ Type + Origin)
ntbt_loglm(xtCars93, ~ Type + Origin)

## so it can be used easily in a pipeline.
Cars93 \%>\%
  ntbt_xtabs(~ Type + Origin) \%>\%
  ntbt_loglm(~ Type + Origin)

## logtrans
## Original function to interface
logtrans(Days ~ Age*Sex*Eth*Lrn, data = quine,
         alpha = seq(0.75, 6.5, len=20))

## The interface reverses the order of data and formula
ntbt_logtrans(data = quine, Days ~ Age*Sex*Eth*Lrn,
              alpha = seq(0.75, 6.5, len=20))

## so it can be used easily in a pipeline.
quine \%>\%
  ntbt_logtrans(Days ~ Age*Sex*Eth*Lrn,
                alpha = seq(0.75, 6.5, len=20))

## polr
op <- options(contrasts = c("contr.treatment", "contr.poly"))

## Original function to interface
polr(Sat ~ Infl + Type + Cont, housing)

## The interface reverses the order of data and formula
ntbt_polr(housing, Sat ~ Infl + Type + Cont)

## so it can be used easily in a pipeline.
housing \%>\%
  ntbt_polr(Sat ~ Infl + Type + Cont)

options(op)

## qda
set.seed(123) ## make reproducible
tr <- sample(1:50, 25)

iris3df <- data.frame(cl = factor(c(rep("s",25), rep("c",25), rep("v",25))),
                      train = rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3]))

## Original function to interface
qda(cl ~ ., iris3df)

## The interface reverses the order of data and formula
ntbt_qda(iris3df, cl ~ .)

## so it can be used easily in a pipeline.
iris3df \%>\%
  ntbt_qda(cl ~ .)

## rlm
## Original function to interface
rlm(stack.loss ~ ., stackloss)

## The interface reverses the order of data and formula
ntbt_rlm(stackloss, stack.loss ~ .)

## so it can be used easily in a pipeline.
stackloss \%>\%
  ntbt_rlm(stack.loss ~ .) \%>\%
  summary()
  
stackloss \%>\%
  ntbt_rlm(stack.loss ~ ., psi = psi.hampel, init = "lts") \%>\%
  summary()

stackloss \%>\%
  ntbt_rlm(stack.loss ~ ., psi = psi.bisquare) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{MASS}
\keyword{corresp}
\keyword{glm.nb}
\keyword{lda}
\keyword{lm.ridge}
\keyword{loglm}
\keyword{polr}
\keyword{qda}
\keyword{rlm}
