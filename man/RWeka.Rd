\name{RWeka}
\alias{ntbt_AdaBoostM1}
\alias{ntbt_Bagging}
\alias{ntbt_CostSensitiveClassifier}
\alias{ntbt_DecisionStump}
\alias{ntbt_Discretize}
\alias{ntbt_GainRatioAttributeEval}
\alias{ntbt_IBk}
\alias{ntbt_InfoGainAttributeEval}
\alias{ntbt_J48}
\alias{ntbt_JRip}
\alias{ntbt_LBR}
\alias{ntbt_LogitBoost}
\alias{ntbt_LinearRegression}
\alias{ntbt_LMT}
\alias{ntbt_Logistic}
\alias{ntbt_M5P}
\alias{ntbt_M5Rules}
\alias{ntbt_MultiBoostAB}
\alias{ntbt_Normalize}
\alias{ntbt_OneR}
\alias{ntbt_PART}
\alias{ntbt_SMO}
\alias{ntbt_Stacking}
\title{Interfaces for RWeka package for data science pipelines.}
\description{
  Interfaces to \code{RWeka} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_AdaBoostM1(data, ...)
ntbt_Bagging(data, ...)
ntbt_CostSensitiveClassifier(data, ...)
ntbt_DecisionStump(data, ...)
ntbt_Discretize(data, ...)
ntbt_GainRatioAttributeEval(data, ...)
ntbt_IBk(data, ...)
ntbt_InfoGainAttributeEval(data, ...)
ntbt_J48(data, ...)
ntbt_JRip(data, ...)
ntbt_LBR(data, ...)
ntbt_LogitBoost(data, ...)
ntbt_LinearRegression(data, ...)
ntbt_LMT(data, ...)
ntbt_Logistic(data, ...)
ntbt_M5P(data, ...)
ntbt_M5Rules(data, ...)
ntbt_MultiBoostAB(data, ...)
ntbt_Normalize(data, ...)
ntbt_OneR(data, ...)
ntbt_PART(data, ...)
ntbt_SMO(data, ...)
ntbt_Stacking(data, ...)
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
library(RWeka)

## R/Weka Attribute Evaluators
## Original function to interface
GainRatioAttributeEval(Species ~ . , data = iris)
InfoGainAttributeEval(Species ~ . , data = iris)

## The interface puts data as first parameter
ntbt_GainRatioAttributeEval(iris, Species ~ .)
ntbt_InfoGainAttributeEval(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_GainRatioAttributeEval(Species ~ .)
iris \%>\%
  ntbt_InfoGainAttributeEval(Species ~ .)

## R/Weka Classifier Functions
data(infert)
infert$STATUS <- factor(infert$case, labels = c("control", "case"))

## Original function to interface
LinearRegression(weight ~ feed, data = chickwts)
Logistic(STATUS ~ spontaneous + induced, data = infert)
SMO(Species ~ ., data = iris, control = Weka_control(K = list("RBFKernel", G = 2)))

## The interface puts data as first parameter
ntbt_LinearRegression(chickwts, weight ~ feed)
ntbt_Logistic(infert, STATUS ~ spontaneous + induced)
ntbt_SMO(iris, Species ~ ., control = Weka_control(K = list("RBFKernel", G = 2)))

## so it can be used easily in a pipeline.
chickwts \%>\%
  ntbt_LinearRegression(weight ~ feed)
infert \%>\%
  ntbt_Logistic(STATUS ~ spontaneous + induced)
iris \%>\%
  ntbt_SMO(Species ~ ., control = Weka_control(K = list("RBFKernel", G = 2)))

## R/Weka Lazy Learners
## No examples provided. LBR seems to need 'lazyBayesianRules'
## and I am too lazy myself to install it
ntbt_IBk(chickwts, weight ~ feed)   ## Example may not make sense


## R/Weka Meta Learners
## MultiBoostAB needs Weka package 'multiBoostAB'
## CostSensitiveClassifier throws an error

## Original function to interface
AdaBoostM1(Species ~ ., data = iris, control = Weka_control(W = "DecisionStump"))
Bagging(Species ~ ., data = iris, control = Weka_control())
LogitBoost(Species ~ ., data = iris, control = Weka_control())
Stacking(Species ~ ., data = iris, control = Weka_control())

## The interface puts data as first parameter
ntbt_AdaBoostM1(iris, Species ~ ., control = Weka_control(W = "DecisionStump"))
ntbt_Bagging(iris, Species ~ ., control = Weka_control())
ntbt_LogitBoost(iris, Species ~ ., control = Weka_control())
ntbt_Stacking(iris, Species ~ ., control = Weka_control())

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_AdaBoostM1(Species ~ ., control = Weka_control(W = "DecisionStump"))
iris \%>\%
  ntbt_Bagging(Species ~ ., control = Weka_control())
iris \%>\%
  ntbt_LogitBoost(Species ~ ., control = Weka_control())
iris \%>\%
  ntbt_Stacking(Species ~ ., control = Weka_control())

## R/Weka Rule Learners
## Original function to interface
JRip(Species ~ ., data = iris)
M5Rules(mpg ~ ., data = mtcars)
OneR(Species ~ ., data = iris)
PART(Species ~ ., data = iris)

## The interface puts data as first parameter
ntbt_JRip(iris, Species ~ .)
ntbt_M5Rules(mtcars, mpg ~ .)
ntbt_OneR(iris, Species ~ .)
ntbt_PART(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_JRip(Species ~ .)
mtcars \%>\%
  ntbt_M5Rules(mpg ~ .)
iris \%>\%
  ntbt_OneR(Species ~ .)
iris \%>\%
  ntbt_PART(Species ~ .)

## R/Weka Classifier Trees
DF3 <- read.arff(system.file("arff", "cpu.arff", package = "RWeka"))
DF4 <- read.arff(system.file("arff", "weather.arff", package = "RWeka"))

## Original function to interface
DecisionStump(play ~ ., data = DF4)
J48(Species ~ ., data = iris)
LMT(play ~ ., data = DF4)
M5P(class ~ ., data = DF3)

## The interface puts data as first parameter
ntbt_DecisionStump(DF4, play ~ .)
ntbt_J48(iris, Species ~ .)
ntbt_LMT(DF4, play ~ .)
ntbt_M5P(DF3, class ~ .)

## so it can be used easily in a pipeline.
DF4 \%>\%
  ntbt_DecisionStump(play ~ .)
iris \%>\%
  ntbt_J48(Species ~ .)
DF4 \%>\%
  ntbt_LMT(play ~ .)
DF3 \%>\%
  ntbt_M5P(class ~ .)

## R/Weka Filters
w <- read.arff(system.file("arff","weather.arff", package = "RWeka"))

## Original function to interface
Discretize(play ~., data = w)
Normalize(~., data = w)

## The interface puts data as first parameter
ntbt_Discretize(w, play ~.)
ntbt_Normalize(w, ~.)

## so it can be used easily in a pipeline.
w \%>\%
  ntbt_Discretize(play ~.)
w \%>\%
  ntbt_Normalize(~.)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{RWeka}
\keyword{AdaBoostM1}
\keyword{Bagging}
\keyword{CostSensitiveClassifier}
\keyword{DecisionStump}
\keyword{Discretize}
\keyword{GainRatioAttributeEval}
\keyword{IBk}
\keyword{InfoGainAttributeEval}
\keyword{J48}
\keyword{JRip}
\keyword{LBR}
\keyword{LogitBoost}
\keyword{LinearRegression}
\keyword{LMT}
\keyword{Logistic}
\keyword{M5P}
\keyword{M5Rules}
\keyword{MultiBoostAB}
\keyword{Normalize}
\keyword{OneR}
\keyword{PART}
\keyword{SMO}
\keyword{Stacking}
