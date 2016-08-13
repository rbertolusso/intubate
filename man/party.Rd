\name{party}
\alias{ntbt_cforest}
\alias{ntbt_ctree}
\alias{ntbt_mob}
\title{Interfaces for party package for data science pipelines.}
\description{
  Interfaces to \code{party} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_cforest(data, ...)
ntbt_ctree(data, ...)
ntbt_mob(data, ...)
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
library(party)

## ntbt_cforest: Random Forest

### honest (i.e., out-of-bag) cross-classification of
### true vs. predicted classes
data("mammoexp", package = "TH.data")
#table(mammoexp$ME, predict(cforest(ME ~ ., data = mammoexp, 
#                                   control = cforest_unbiased(ntree = 50)),
#                           OOB = TRUE))

## Original function to interface
set.seed(290875)
cforest(ME ~ ., data = mammoexp, control = cforest_unbiased(ntree = 50))

## The interface puts data as first parameter
set.seed(290875)
ntbt_cforest(mammoexp, ME ~ ., control = cforest_unbiased(ntree = 50))

## so it can be used easily in a pipeline.
set.seed(290875)
mammoexp \%>\%
  ntbt_cforest(ME ~ ., control = cforest_unbiased(ntree = 50))

## ntbt_ctree: Conditional Inference Trees
airq <- subset(airquality, !is.na(Ozone))

## Original function to interface
set.seed(290875)
ctree(Ozone ~ ., data = airq, controls = ctree_control(maxsurrogate = 3))

## The interface puts data as first parameter
set.seed(290875)
ntbt_ctree(airq, Ozone ~ ., controls = ctree_control(maxsurrogate = 3))

## so it can be used easily in a pipeline.
set.seed(290875)
airq \%>\%
  ntbt_ctree(Ozone ~ ., controls = ctree_control(maxsurrogate = 3))


## ntbt_mob: Model-based Recursive Partitioning
data("BostonHousing", package = "mlbench")
## and transform variables appropriately (for a linear regression)
BostonHousing$lstat <- log(BostonHousing$lstat)
BostonHousing$rm <- BostonHousing$rm^2
## as well as partitioning variables (for fluctuation testing)
BostonHousing$chas <- factor(BostonHousing$chas, levels = 0:1, 
                             labels = c("no", "yes"))
BostonHousing$rad <- factor(BostonHousing$rad, ordered = TRUE)

## Original function to interface
set.seed(290875)
mob(medv ~ lstat + rm | zn + indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
    control = mob_control(minsplit = 40), data = BostonHousing, 
    model = linearModel)

## The interface puts data as first parameter
set.seed(290875)
ntbt_mob(BostonHousing, 
         medv ~ lstat + rm | zn + indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
         control = mob_control(minsplit = 40), model = linearModel)

## so it can be used easily in a pipeline.
set.seed(290875)
BostonHousing \%>\%
  ntbt_mob(medv ~ lstat + rm | zn + indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
           control = mob_control(minsplit = 40), model = linearModel)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{party}
\keyword{cforest}
\keyword{ctree}
\keyword{mob}
