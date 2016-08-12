\name{intubate-package}
\alias{intubate-package}
\docType{package}
\title{Interface to Popular R Functions for Data Science Pipelines.}
\description{
  \code{intubate} implements interfaces to popular R functions with formulas,
  such as \code{lm}, so they can be included painlessly in data science pipelines
  implemented by \code{magrittr} with the operator \%>\%.
  
  \code{intubate} allows to use, effortlessly,
  classic R functions in a consistent pipeline notation.
  
  For functions missing interfaces, there is an easy
  (one line) way to to create new interfaces "on demand" 
  (see \code{\link{intubate}} or \code{\link{ntbt}}) that should cover
  the vast majority of functions.
  
  Currently, there are 160 interfaces for:
  
  \code{caret}: Classification and Regression Training
  
  \code{e1071}: Support Vector Machines

  \code{gam}: Generalized Additive Models
  
  \code{gbm}: Generalized Boosted Regression Models
  
  \code{graphics}: The R Graphics Package

  \code{Hmisc}: Harrell Miscellaneous

  \code{iRegression}: Regression Methods for Interval-Valued Variables
  
  \code{kernlab}: Kernel-Based Machine Learning Lab

  \code{lattice}: Trellis Graphics for R
  
  \code{leaps}: Regression Subset Selection
  
  \code{lfe}: Linear Group Fixed Effects

  \code{MASS}: Robust Regression, Linear Discriminant Analysis, Ridge Regression,
               Probit Regression, ...
  
  \code{nlme}: Linear and Nonlinear Mixed Effects Models
  
  \code{nnet}: Feed-Forward Neural Networks and Multinomial Log-Linear Models
  
  \code{pls}: Partial Least Squares and Principal Component Regression
  
  \code{party}: A Laboratory for Recursive Partytioning

  \code{randomForest}: Random Forests for Classification and Regression
  
  \code{rms}: Regression Modeling Strategies
  
  \code{rpart}: Recursive Partitioning and Regression Trees
  
  \code{stats}: The R Stats Package
  
  \code{survey}: Analysis of Complex Survey Samples

  \code{survival}: Survival Analysis
  
  \code{tree}: Classification and Regression Trees
  
  The aim is to provide interfaces to most methodologies
  used in data science.
  
  \code{intubate} itself only needs the \code{base} library.
  To keep it as lean as possible, \code{intubate} will not
  install not load any library. You need to make sure that the
  library containing the functions to be interfaced are loaded
  (before or after \code{intubate}). Moreover, you can interface
  the functions of any library directly without the need to
  create interfaces (see \code{\link{ntbt}}) so perhaps in the
  future that will be the preferred way of using \code{intubate}.

  \code{intubate} is still a work in progress.
  As such, the implementation may change in future
  versions until stabilization.
}
\details{
\tabular{ll}{
Package: \tab intubate\cr
Type: \tab Package\cr
Version: \tab 0.99.3\cr
Date: \tab 2016-08-07\cr
License: \tab GPL (>=2)\cr
}
See examples of use below.
}
\author{
Roberto Bertolusso

Maintainer: Roberto Bertolusso <rbertolusso@rice.edu>
}
\examples{
\dontrun{
######### Interface to lm #########
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

######### Interface to cor.test #########
## Original function to interface
cor.test(~ CONT + INTG, data = USJudgeRatings)

## The interface reverses the order of data and formula
ntbt_cor.test(data = USJudgeRatings, ~ CONT + INTG)

## so it can be used easily in a pipeline.
USJudgeRatings \%>\%
  ntbt_cor.test(~ CONT + INTG)
  
######### Interfaces to aggregate and xtabs #########
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
\seealso{
  \code{\link{intubate}}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{data science}

