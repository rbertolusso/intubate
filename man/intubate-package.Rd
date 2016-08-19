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
  
  Currently, there are 416 interfaces for:
  
  \code{adabag}: Multiclass AdaBoost.M1, SAMME and Bagging
  
  \code{aod}: Analysis of Overdispersed Data

  \code{ape}: Analyses of Phylogenetics and Evolution

  \code{arm}: Data Analysis Using Regression and Multilevel/Hierarchical Models

  \code{brglm}: Bias reduction in binomial-response generalized linear models

  \code{caper}: Comparative Analyses of Phylogenetics and Evolution in R
  
  \code{car}: Companion to Applied Regression
  
  \code{caret}: Classification and Regression Training
  
  \code{coin}: Conditional Inference Procedures in a Permutation Test Framework
  
  \code{CORElearn}: Classification, Regression and Feature Evaluation
  
  \code{drc}: Analysis of Dose-Response Curves

  \code{e1071}: Support Vector Machines

  \code{earth}: Multivariate Adaptive Regression Splines
  
  \code{EnvStats}: Environmental Statistics, Including US EPA Guidance
  
  \code{fGarch}: Rmetrics - Autoregressive Conditional Heteroskedastic Modelling

  \code{forecast}: Forecasting Functions for Time Series and Linear Models
  
  \code{gam}: Generalized Additive Models
  
  \code{gbm}: Generalized Boosted Regression Models
  
  \code{gee}: Generalized Estimation Equation Solver
  
  \code{glmnet}: Lasso and Elastic-Net Regularized Generalized Linear Models
  
  \code{glmx}: Generalized Linear Models Extended
  
  \code{gmnl}: Multinomial Logit Models with Random Parameters
  
  \code{gplots}: Various R Programming Tools for Plotting Data
  
  \code{graphics}: The R Graphics Package
  
  \code{gss}: General Smoothing Splines

  \code{Hmisc}: Harrell Miscellaneous

  \code{ipred}: Improved Predictors

  \code{iRegression}: Regression Methods for Interval-Valued Variables
  
  \code{kernlab}: Kernel-Based Machine Learning Lab

  \code{klaR}: Classification and Visualization

  \code{lattice}: Trellis Graphics for R
  
  \code{latticeExtra}: Extra Graphical Utilities Based on Lattice
  
  \code{leaps}: Regression Subset Selection
  
  \code{lfe}: Linear Group Fixed Effects

  \code{lme4}: Linear Mixed-Effects Models using 'Eigen' and S4
  
  \code{lmtest}: Testing Linear Regression Models

  \code{MASS}: Robust Regression, Linear Discriminant Analysis, Ridge Regression,
               Probit Regression, ...
               
  \code{MCMCglmm}: MCMC Generalised Linear Mixed Models

  \code{mda}: Mixture and Flexible Discriminant Analysis
  
  \code{metafor}: Meta-Analysis Package for R
  
  \code{mgcv}: Mixed GAM Computation Vehicle with GCV/AIC/REML Smoothness Estimation
  
  \code{mhurdle}: Multiple Hurdle Tobit Models
  
  \code{minpack.lm}: R Interface to the Levenberg-Marquardt Nonlinear Least-Squares
                     Algorithm Found in MINPACK, Plus Support for Bounds
                     
  \code{mlogit}: Multinomial logit model

  \code{modeltools}: Tools and Classes for Statistical Models
  
  \code{nlme}: Linear and Nonlinear Mixed Effects Models
  
  \code{nlreg}: Higher Order Inference for Nonlinear Heteroscedastic Models
  
  \code{nnet}: Feed-Forward Neural Networks and Multinomial Log-Linear Models
  
  \code{ordinal}: Regression Models for Ordinal Data
  
  \code{party}: A Laboratory for Recursive Partytioning

  \code{plotrix}: Various Plotting Functions
  
  \code{pls}: Partial Least Squares and Principal Component Regression
  
  \code{pROC}: Display and Analyze ROC Curves
  
  \code{pscl}: Political Science Computational Laboratory, Stanford University
  
  \code{quantreg}: Quantile Regression

  \code{randomForest}: Random Forests for Classification and Regression
  
  \code{Rchoice}: Discrete Choice (Binary, Poisson and Ordered) Models with Random Parameters

  \code{rminer}: Data Mining Classification and Regression Methods 
  
  \code{rms}: Regression Modeling Strategies
  
  \code{robustbase}: Basic Robust Statistics
  
  \code{rpart}: Recursive Partitioning and Regression Trees
  
  \code{RRF}: Regularized Random Forest
  
  \code{RWeka}: R/Weka Interface
  
  \code{sem}: Structural Equation Models
  
  \code{spBayes}: Univariate and Multivariate Spatial-temporal Modeling

  \code{stats}: The R Stats Package (glm, lm, loess, lqs, nls, ...)
  
  \code{strucchange}: Testing, Monitoring, and Dating Structural Changes

  \code{survey}: Analysis of Complex Survey Samples

  \code{survival}: Survival Analysis
  
  \code{SwarmSVM}: Ensemble Learning Algorithms Based on Support Vector Machines
  
  \code{tree}: Classification and Regression Trees
  
  \code{vcd}: Visualizing Categorical Data
  
  \code{vegan}: Community Ecology Package
  
  The aim is to provide interfaces to most methodologies
  used in data science.
  
  \code{intubate} core depends only on \code{base}, \code{stats}, 
  and \code{utils} libraries.
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
library(intubate)
library(magrittr)

######### Interface to lm #########
## Original function to interface
lm(conc ~ uptake, CO2)

## The interface reverses the order of data and formula
ntbt_lm(CO2, conc ~ uptake)

## so it can be used easily in a pipeline.
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

