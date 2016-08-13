\name{klaR}
\alias{ntbt_classscatter}
\alias{ntbt_cond.index}
\alias{ntbt_greedy.wilks}
\alias{ntbt_loclda}
\alias{ntbt_meclight}
\alias{ntbt_NaiveBayes}
\alias{ntbt_nm}
\alias{ntbt_partimat}
\alias{ntbt_plineplot}
\alias{ntbt_pvs}
\alias{ntbt_rda}
\alias{ntbt_sknn}
\alias{ntbt_stepclass}
\alias{ntbt_woe}
\title{Interfaces for klaR package for data science pipelines.}
\description{
  Interfaces to \code{klaR} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_classscatter(data, ...)
ntbt_cond.index(data, ...)
ntbt_greedy.wilks(data, ...)
ntbt_loclda(data, ...)
ntbt_meclight(data, ...)
ntbt_NaiveBayes(data, ...)
ntbt_nm(data, ...)
ntbt_partimat(data, ...)
ntbt_plineplot(data, ...)
ntbt_pvs(data, ...)
ntbt_rda(data, ...)
ntbt_sknn(data, ...)
ntbt_stepclass(data, ...)
ntbt_woe(data, ...)
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
library(klaR)

## ntbt_classscatter: Classification scatterplot matrix
data(B3)
library(MASS)

## Original function to interface
classscatter(PHASEN ~ BSP91JW + EWAJW + LSTKJW, data = B3, method = "lda")

## The interface puts data as first parameter
ntbt_classscatter(B3, PHASEN ~ BSP91JW + EWAJW + LSTKJW, method = "lda")

## so it can be used easily in a pipeline.
B3 \%>\%
  ntbt_classscatter(PHASEN ~ BSP91JW + EWAJW + LSTKJW, method = "lda")


## ntbt_cond.index: Calculation of Condition Indices for Linear Regression
data(Boston)

## Original function to interface
cond.index(medv ~ ., data = Boston)

## The interface puts data as first parameter
ntbt_cond.index(Boston, medv ~ .)

## so it can be used easily in a pipeline.
Boston \%>\%
  ntbt_cond.index(medv ~ .)


## ntbt_greedy.wilks: Stepwise forward variable selection for classification
data(B3)

## Original function to interface
greedy.wilks(PHASEN ~ ., data = B3, niveau = 0.1)

## The interface puts data as first parameter
ntbt_greedy.wilks(B3, PHASEN ~ ., niveau = 0.1)

## so it can be used easily in a pipeline.
B3 \%>\%
  ntbt_greedy.wilks(PHASEN ~ ., niveau = 0.1)

## ntbt_loclda: Localized Linear Discriminant Analysis (LocLDA)
## Original function to interface
loclda(PHASEN ~ ., data = B3)

## The interface puts data as first parameter
ntbt_loclda(B3, PHASEN ~ .)

## so it can be used easily in a pipeline.
B3 \%>\%
  ntbt_loclda(PHASEN ~ .)


## ntbt_meclight: Minimal Error Classification
data(iris)

## Original function to interface
meclight(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_meclight(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_meclight(Species ~ .)


## ntbt_NaiveBayes: Naive Bayes Classifier
data(iris)

## Original function to interface
NaiveBayes(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_NaiveBayes(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_NaiveBayes(Species ~ .)


## ntbt_nm: Nearest Mean Classification
## Original function to interface
nm(PHASEN ~ ., data = B3)

## The interface puts data as first parameter
ntbt_nm(B3, PHASEN ~ .)

## so it can be used easily in a pipeline.
B3 \%>\%
  ntbt_nm(PHASEN ~ .)


## ntbt_partimat: Plotting the 2-d partitions of classification methods
## Original function to interface
partimat(Species ~ ., data = iris, method = "lda")

## The interface puts data as first parameter
ntbt_partimat(iris, Species ~ ., method = "lda")

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_partimat(Species ~ ., method = "lda")


## ntbt_plineplot: Plotting marginal posterior class probabilities
## Original function to interface
plineplot(PHASEN ~ ., data = B3, method = "lda", x = "EWAJW", xlab = "EWAJW")

## The interface puts data as first parameter
ntbt_plineplot(B3, PHASEN ~ ., method = "lda", x = "EWAJW", xlab = "EWAJW")

## so it can be used easily in a pipeline.
B3 \%>\%
  ntbt_plineplot(PHASEN ~ ., method = "lda", x = "EWAJW", xlab = "EWAJW")


## ntbt_pvs: Pairwise variable selection for classification
library("mlbench")
data("Satellite")

## Original function to interface
pvs(classes ~ ., Satellite[1:3218,], method="qda", vs.method="ks.test")

## The interface puts data as first parameter
ntbt_pvs(Satellite[1:3218,], classes ~ ., method="qda", vs.method="ks.test")

## so it can be used easily in a pipeline.
Satellite[1:3218,] \%>\%
  ntbt_pvs(classes ~ ., method="qda", vs.method="ks.test")


## ntbt_rda: Regularized Discriminant Analysis (RDA)
## Original function to interface
rda(Species ~ ., data = iris, gamma = 0.05, lambda = 0.2)

## The interface puts data as first parameter
ntbt_rda(iris, Species ~ ., gamma = 0.05, lambda = 0.2)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_rda(Species ~ ., gamma = 0.05, lambda = 0.2)


## ntbt_sknn: Simple k nearest Neighbours
## Original function to interface
sknn(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_sknn(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_sknn(Species ~ .)


## ntbt_stepclass: Stepwise variable selection for classification
## Original function to interface
stepclass(Species ~ ., data = iris, method = "qda", 
          start.vars = "Sepal.Width", criterion = "AS")  # same as above 

## The interface puts data as first parameter
ntbt_stepclass(iris, Species ~ ., method = "qda", 
               start.vars = "Sepal.Width", criterion = "AS")  # same as above 

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_stepclass(Species ~ ., method = "qda", 
                 start.vars = "Sepal.Width", criterion = "AS")  # same as above 


## ntbt_woe: Weights of evidence
data("GermanCredit")
set.seed(6)
train <- sample(nrow(GermanCredit), round(0.6*nrow(GermanCredit)))

## Original function to interface
woe(credit_risk ~ ., data = GermanCredit[train,], zeroadj = 0.5, applyontrain = TRUE)

## The interface puts data as first parameter
ntbt_woe(GermanCredit[train,], credit_risk ~ ., zeroadj = 0.5, applyontrain = TRUE)

## so it can be used easily in a pipeline.
GermanCredit[train,] \%>\%
  ntbt_woe(credit_risk ~ ., zeroadj = 0.5, applyontrain = TRUE)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{klaR}
\alias{classscatter}
\alias{cond.index}
\alias{greedy.wilks}
\alias{loclda}
\alias{meclight}
\alias{NaiveBayes}
\alias{nm}
\alias{partimat}
\alias{plineplot}
\alias{pvs}
\alias{rda}
\alias{sknn}
\alias{stepclass}
\alias{woe}
