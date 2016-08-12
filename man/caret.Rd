\name{caret}
\alias{ntbt_avNNet}
\alias{ntbt_bagEarth}
\alias{ntbt_bagFDA}
\alias{ntbt_calibration}
\alias{ntbt_dummyVars}
\alias{ntbt_icr}
\alias{ntbt_knn3}
\alias{ntbt_lift}
\alias{ntbt_pcaNNet}
\alias{ntbt_sbf}
\alias{ntbt_train}
\title{Interfaces for caret package for data science pipelines.}
\description{
  Interfaces to \code{caret} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_avNNet(data, ...)
ntbt_bagEarth(data, ...)
ntbt_bagFDA(data, ...)
ntbt_calibration(data, ...)
ntbt_dummyVars(data, ...)
ntbt_icr(data, ...)
ntbt_knn3(data, ...)
ntbt_lift(data, ...)
ntbt_pcaNNet(data, ...)
ntbt_sbf(data, ...)
ntbt_train(data, ...)
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
library(caret)


## ntbt_avNNet: Neural Networks Using Model Averaging
## Not found example using formula interface, and I am
## completely ignorant to construct one.
data(BloodBrain)
BB <- list(bbbDescr, logBBB)

## Original function to interface
avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)

## The interface puts data as first parameter
ntbt_avNNet(BB, bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)

## so it can be used easily in a pipeline.
BB \%>\%
  ntbt_avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)


## ntbt_bagEarth: Bagged Earth

## Original function to interface
bagEarth(Volume ~ ., data = trees)

## The interface puts data as first parameter
ntbt_bagEarth(trees, Volume ~ .)

## so it can be used easily in a pipeline.
trees \%>\%
  ntbt_bagEarth(Volume ~ .)


## ntbt_bagFDA: Bagged FDA
library(mlbench)
library(earth)
data(Glass)

set.seed(36)
inTrain <- sample(1:dim(Glass)[1], 150)

trainData <- Glass[ inTrain, ]
testData  <- Glass[-inTrain, ]
## Original function to interface
## bagFDA(Type ~ ., trainData)   ## There is an error:
## Error in requireNamespaceQuietStop("mda") : package mda is required
##                               ## even when mda is installed
## For now all of this stays commented.

## The interface puts data as first parameter
## ntbt_bagFDA(trainData, Type ~ .)

## so it can be used easily in a pipeline.
## trainData \%>\%
##   ntbt_bagFDA(Type ~ .)


## ntbt_calibration: Probability Calibration Plot
data(mdrr)
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .5)]
inTrain <- createDataPartition(mdrrClass)
trainX <- mdrrDescr[inTrain[[1]], ]
trainY <- mdrrClass[inTrain[[1]]]
testX <- mdrrDescr[-inTrain[[1]], ]
testY <- mdrrClass[-inTrain[[1]]]
library(MASS)
ldaFit <- lda(trainX, trainY)
qdaFit <- qda(trainX, trainY)
testProbs <- data.frame(obs = testY,
lda <- predict(ldaFit, testX)$posterior[,1],
qda <- predict(qdaFit, testX)$posterior[,1])

## Original function to interface
calPlotData <- calibration(obs ~ lda + qda, data = testProbs)
xyplot(calPlotData, auto.key = list(columns = 2))

## The interface puts data as first parameter
calPlotData <- ntbt_calibration(testProbs, obs ~ lda + qda)
xyplot(calPlotData, auto.key = list(columns = 2))

## so it can be used easily in a pipeline.
testProbs \%>\%
  ntbt_calibration(obs ~ lda + qda) \%>\%
  xyplot(auto.key = list(columns = 2))


## ntbt_dummyVars
when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"))

levels(when$time) <- list(morning="morning",
                          afternoon="afternoon",
                          night="night")
levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")

## Original function to interface
mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when[1:3,])

## The interface puts data as first parameter
mainEffects <- ntbt_dummyVars(when, ~ day + time)
mainEffects
predict(mainEffects, when[1:3,])

## so it can be used easily in a pipeline.
when \%>\%
  ntbt_dummyVars(~ day + time) \%>\%
  predict(when[1:3,])


## ntbt_icr: Independent Component Regression
## Not found example using formula interface, and I am
## completely ignorant to construct one.
data(BloodBrain)
BB <- list(bbbDescr, logBBB)

## Original function to interface
icr(bbbDescr, logBBB, n.comp = 5)

## The interface puts data as first parameter
ntbt_icr(BB, bbbDescr, logBBB, n.comp = 5)

## so it can be used easily in a pipeline.
BB \%>\%
  ntbt_icr(bbbDescr, logBBB, n.comp = 5)


## ntbt_knn3: k-Nearest Neighbour Classification
## Original function to interface
knn3(Species ~ ., iris)

## The interface puts data as first parameter
ntbt_knn3(iris, Species ~ .)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_knn3(Species ~ .)


## ntbt_lift: Lift Plot
set.seed(1)
simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
perfect = sort(runif(200), decreasing = TRUE),
random = runif(200))
## Original function to interface
lift1 <- lift(obs ~ random, data = simulated)
lift1
xyplot(lift1)

## The interface puts data as first parameter
lift1 <- ntbt_lift(simulated, obs ~ random)
lift1
xyplot(lift1)

## so it can be used easily in a pipeline.
simulated \%>\%
  ntbt_lift(obs ~ random) \%>\%
  xyplot()


## ntbt_pcaNNet: Neural Networks with a Principal Component Step
## Not found example using formula interface, and I am
## completely ignorant to construct one.
data(BloodBrain)
BB <- list(bbbDescr, logBBB)

## Original function to interface
pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)

## The interface puts data as first parameter
ntbt_pcaNNet(BB, bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)

## so it can be used easily in a pipeline.
BB \%>\%
  ntbt_pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)


## ntbt_sbf: Selection By Filtering (SBF)
## Not found example using formula interface, and I am
## completely ignorant to construct one.
data(BloodBrain)
BB <- list(bbbDescr, logBBB)

## Be prepared to wait...
## Original function to interface
sbf(bbbDescr, logBBB,
    sbfControl = sbfControl(functions = rfSBF,
                            verbose = FALSE, 
                            method = "cv"))

## The interface puts data as first parameter
ntbt_sbf(BB, bbbDescr, logBBB,
         sbfControl = sbfControl(functions = rfSBF,
                                 verbose = FALSE, 
                                 method = "cv"))

## so it can be used easily in a pipeline.
BB \%>\%
  ntbt_sbf(bbbDescr, logBBB,
           sbfControl = sbfControl(functions = rfSBF,
                                   verbose = FALSE, 
                                   method = "cv"))


## ntbt_train: Fit Predictive Models over Different Tuning Parameters
library(mlbench)
data(BostonHousing)

## Original function to interface
train(medv ~ . + rm:lstat, data = BostonHousing, method = "lm")

## The interface puts data as first parameter
ntbt_train(BostonHousing, medv ~ . + rm:lstat, method = "lm")

## so it can be used easily in a pipeline.
BostonHousing \%>\%
  ntbt_train(medv ~ . + rm:lstat, method = "lm")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{caret}
\keyword{ntbt_avNNet}
\keyword{ntbt_bagEarth}
\keyword{ntbt_bagFDA}
\keyword{ntbt_calibration}
\keyword{ntbt_dummyVars}
\keyword{ntbt_icr}
\keyword{ntbt_knn3}
\keyword{ntbt_lift}
\keyword{ntbt_pcaNNet}
\keyword{ntbt_sbf}
\keyword{ntbt_train}
