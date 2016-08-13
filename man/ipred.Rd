\name{ipred}
\alias{ntbt_bagging}
\alias{ntbt_errorest}
\alias{ntbt_inbagg}
\alias{ntbt_inclass}
\alias{ntbt_slda}
\title{Interfaces for ipred package for data science pipelines.}
\description{
  Interfaces to \code{ipred} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bagging(data, ...)
ntbt_errorest(data, ...)
ntbt_inbagg(data, ...)
ntbt_inclass(data, ...)
ntbt_slda(data, ...)
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
library(ipred)

## ntbt_bagging: Bagging Classification, Regression and Survival Trees
data("BreastCancer", package = "mlbench")

## Original function to interface
bagging(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size
        + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data=BreastCancer, coob=TRUE)

## The interface puts data as first parameter
ntbt_bagging(BreastCancer, 
             Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size
             + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, coob=TRUE)

## so it can be used easily in a pipeline.
BreastCancer \%>\%
  ntbt_bagging(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size
               + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, coob=TRUE)


## ntbt_errorest: Estimators of Prediction Error
data("iris")
library("MASS")
mypredict.lda <- function(object, newdata)
  predict(object, newdata = newdata)$class

## Original function to interface
errorest(Species ~ ., data = iris, model = lda, estimator = "cv", predict = mypredict.lda)

## The interface puts data as first parameter
ntbt_errorest(iris, Species ~ ., model = lda, estimator = "cv", predict = mypredict.lda)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_errorest(Species ~ ., model = lda, estimator = "cv", predict = mypredict.lda)


## ntbt_inbagg: Indirect Bagging
library("MASS")
library("rpart")
y <- as.factor(sample(1:2, 100, replace = TRUE))
W <- mvrnorm(n = 200, mu = rep(0, 3), Sigma = diag(3))
X <- mvrnorm(n = 200, mu = rep(2, 3), Sigma = diag(3))
colnames(W) <- c("w1", "w2", "w3")
colnames(X) <- c("x1", "x2", "x3")
DATA <- data.frame(y, W, X)
pFUN <- list(list(formula = w1~x1+x2, model = lm, predict = mypredict.lm),
list(model = rpart))

## Original function to interface
inbagg(y ~ w1 + w2 + w3 ~ x1 + x2 + x3, data = DATA, pFUN = pFUN)

## The interface puts data as first parameter
ntbt_inbagg(DATA, y ~ w1 + w2 + w3 ~ x1 + x2 + x3, pFUN = pFUN)

## so it can be used easily in a pipeline.
DATA \%>\%
  ntbt_inbagg(y ~ w1 + w2 + w3 ~ x1 + x2 + x3, pFUN = pFUN)


## ntbt_inclass: Indirect Classification
data("Smoking", package = "ipred")
# Set three groups of variables:
# 1) explanatory variables are: TarY, NicY, COY, Sex, Age
# 2) intermediate variables are: TVPS, BPNL, COHB
# 3) response (resp) is defined by:
classify <- function(data) {
  data <- data[,c("TVPS", "BPNL", "COHB")]
  res <- t(t(data) > c(4438, 232.5, 58))
  res <- as.factor(ifelse(apply(res, 1, sum) > 2, 1, 0))
  res
}
response <- classify(Smoking[ ,c("TVPS", "BPNL", "COHB")])
smoking <- data.frame(Smoking, response)

## Original function to interface
inclass(response ~ TVPS + BPNL + COHB ~ TarY + NicY + COY + Sex + Age, data = smoking,
        pFUN = list(list(model = lm, predict = mypredict.lm)), cFUN = classify)

## The interface puts data as first parameter
ntbt_inclass(smoking, response ~ TVPS + BPNL + COHB ~ TarY + NicY + COY + Sex + Age,
             pFUN = list(list(model = lm, predict = mypredict.lm)), cFUN = classify)

## so it can be used easily in a pipeline.
smoking \%>\%
  ntbt_inclass(response ~ TVPS + BPNL + COHB ~ TarY + NicY + COY + Sex + Age,
               pFUN = list(list(model = lm, predict = mypredict.lm)), cFUN = classify)


## ntbt_slda: Stabilised Linear Discriminant Analysis
library("mlbench")
library("MASS")
learn <- as.data.frame(mlbench.twonorm(100))

## Original function to interface
slda(classes ~ ., data=learn)

## The interface puts data as first parameter
ntbt_slda(learn, classes ~ .)

## so it can be used easily in a pipeline.
learn \%>\%
  ntbt_slda(classes ~ .)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ipred}
\keyword{bagging}
\keyword{errorest}
\keyword{inbagg}
\keyword{inclass}
\keyword{slda}
