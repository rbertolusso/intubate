\name{pROC}
\alias{ntbt_auc}
\alias{ntbt_ci}
\alias{ntbt_ci.auc}
\alias{ntbt_ci.coords}
\alias{ntbt_ci.se}
\alias{ntbt_ci.sp}
\alias{ntbt_ci.thresholds}
\alias{ntbt_multiclass.roc}
\alias{ntbt_plot.roc}
\alias{ntbt_roc}
\title{Interfaces for pROC package for data science pipelines.}
\description{
  Interfaces to \code{pROC} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_auc(data, ...)
ntbt_ci(data, ...)
ntbt_ci.auc(data, ...)
ntbt_ci.coords(data, ...)
ntbt_ci.se(data, ...)
ntbt_ci.sp(data, ...)
ntbt_ci.thresholds(data, ...)
ntbt_multiclass.roc(data, ...)
ntbt_plot.roc(data, ...)
ntbt_roc(data, ...)
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
library(pROC)

## NOTE: pROC examples below use both formula and non-formula variants.
##       In examples for other packages, almost always only
##       the formula variant is shown, but in those cases also
##       the non-formula variants should work.

## ntbt_auc: Compute the area under the ROC curve
data(aSAH)

## Original function to interface
auc(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
auc(outcome, s100b)
detach()
## or use $
auc(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
## NOTE: in this case the formula version fails, and I have found no
##       way to trick auc into accepting the formula (so far).
##       Maybe (only maybe) there is a problem with auc, as formula
##       variant may not be used, so it was probably not
##       reported as a bug before. The rest of the interfaced
##       functions seem to work fine.
## ntbt_auc(data = aSAH, outcome ~ s100baSAH)
## The non-formula variant works fine
ntbt_auc(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
#aSAH \%>\%
#  ntbt_auc(outcome ~ s100baSAH)
aSAH \%>\%
  ntbt_auc(outcome, s100b)


## ntbt_ci: Compute the confidence interval of a ROC curve
## Original function to interface
ci(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
ci(outcome, s100b)
detach()
## or use $
ci(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
ntbt_ci(aSAH, outcome ~ s100b)
ntbt_ci(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
aSAH \%>\%
  ntbt_ci(outcome ~ s100b)
aSAH \%>\%
  ntbt_ci(outcome, s100b)


## ci.auc: Compute the confidence interval of the AUC
## Original function to interface
ci.auc(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
ci.auc(outcome, s100b)
detach()
## or use $
ci.auc(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
ntbt_ci.auc(aSAH, outcome ~ s100b)
ntbt_ci.auc(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
aSAH \%>\%
  ntbt_ci.auc(outcome ~ s100b)
aSAH \%>\%
  ntbt_ci.auc(outcome, s100b)


## ntbt_ci.coords: Compute the confidence interval of arbitrary coordinates
## Original function to interface
set.seed(1)
ci.coords(outcome ~ s100b, data = aSAH, x="best", input = "threshold", 
          ret=c("specificity", "ppv", "tp"))
set.seed(1)
ci.coords(aSAH$outcome, aSAH$s100b, x="best", input = "threshold", 
          ret=c("specificity", "ppv", "tp"))
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
set.seed(1)
ci.coords(outcome, s100b, x="best", input = "threshold", 
          ret=c("specificity", "ppv", "tp"))
detach()
## or use $
set.seed(1)
ci.coords(aSAH$outcome, aSAH$s100b, x="best", input = "threshold", 
          ret=c("specificity", "ppv", "tp"))

## The interface puts data as first parameter
set.seed(1)
ntbt_ci.coords(aSAH, outcome ~ s100b, x="best", input = "threshold", 
               ret=c("specificity", "ppv", "tp"))
set.seed(1)
ntbt_ci.coords(aSAH, outcome, s100b, x="best", input = "threshold", 
               ret=c("specificity", "ppv", "tp"))

## so it can be used easily in a pipeline.
set.seed(1)
aSAH \%>\%
  ntbt_ci.coords(outcome ~ s100b, x="best", input = "threshold", 
                 ret=c("specificity", "ppv", "tp"))
set.seed(1)
aSAH \%>\%
  ntbt_ci.coords(outcome, s100b, x="best", input = "threshold", 
                 ret=c("specificity", "ppv", "tp"))


## ntbt_ci.se: Compute the confidence interval of sensitivities at given specificities
## Original function to interface
set.seed(1)
ci.se(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
set.seed(1)
ci.se(outcome, s100b)
detach()
## or use $
set.seed(1)
ci.se(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
set.seed(1)
ntbt_ci.se(aSAH, outcome ~ s100b)
set.seed(1)
ntbt_ci.se(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
set.seed(1)
aSAH \%>\%
  ntbt_ci.se(outcome ~ s100b)
set.seed(1)
aSAH \%>\%
  ntbt_ci.se(outcome, s100b)


## ntbt_ci.sp: Compute the confidence interval of specificities at given sensitivities
## Original function to interface
set.seed(1)
ci.sp(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
set.seed(1)
ci.sp(outcome, s100b)
detach()
## or use $
set.seed(1)
ci.sp(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
set.seed(1)
ntbt_ci.sp(aSAH, outcome ~ s100b)
set.seed(1)
ntbt_ci.sp(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
set.seed(1)
aSAH \%>\%
  ntbt_ci.sp(outcome ~ s100b, x="best", input = "threshold", 
             ret=c("specificity", "ppv", "tp"))
set.seed(1)
aSAH \%>\%
  ntbt_ci.sp(outcome, s100b, x="best", input = "threshold", 
             ret=c("specificity", "ppv", "tp"))


## ntbt_ci.thresholds: Compute the confidence interval of thresholds
## Original function to interface
set.seed(1)
ci.thresholds(outcome ~ s100b, data = aSAH)
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
set.seed(1)
ci.thresholds(outcome, s100b)
detach()
## or use $
set.seed(1)
ci.thresholds(aSAH$outcome, aSAH$s100b)

## The interface puts data as first parameter
set.seed(1)
ntbt_ci.thresholds(aSAH, outcome ~ s100b)
set.seed(1)
ntbt_ci.thresholds(aSAH, outcome, s100b)

## so it can be used easily in a pipeline.
set.seed(1)
aSAH \%>\%
  ntbt_ci.thresholds(outcome ~ s100b)
set.seed(1)
aSAH \%>\%
  ntbt_ci.thresholds(outcome, s100b)


## ntbt_multiclass.roc: Multi-clmulticlass.roc Multi-class AUCass AUC
## Original function to interface
multiclass.roc(gos6 ~ s100b, data = aSAH, levels = c(3, 4, 5))
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
multiclass.roc(gos6, s100b, levels = c(3, 4, 5))
detach()
## or use $
multiclass.roc(aSAH$gos6, aSAH$s100b, levels = c(3, 4, 5))

## The interface puts data as first parameter
ntbt_multiclass.roc(aSAH, gos6 ~ s100b, levels = c(3, 4, 5))
ntbt_multiclass.roc(aSAH, gos6, s100b, levels = c(3, 4, 5))

## so it can be used easily in a pipeline.
aSAH \%>\%
  ntbt_multiclass.roc(gos6 ~ s100b, levels = c(3, 4, 5))
aSAH \%>\%
  ntbt_multiclass.roc(gos6, s100b, levels = c(3, 4, 5))


## ntbt_plot.roc: Plot a ROC curve
## Original function to interface
plot.roc(outcome ~ s100b, data = aSAH, type="b", pch=21, col="blue", bg="grey")
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
plot.roc(outcome, s100b, type="b", pch=21, col="blue", bg="grey")
detach()
## or use $
plot.roc(aSAH$outcome, aSAH$s100b, type="b", pch=21, col="blue", bg="grey")

## The interface puts data as first parameter
ntbt_plot.roc(aSAH, outcome ~ s100b, type="b", pch=21, col="blue", bg="grey")
ntbt_plot.roc(aSAH, outcome, s100b, type="b", pch=21, col="blue", bg="grey")

## so it can be used easily in a pipeline.
aSAH \%>\%
  ntbt_plot.roc(outcome ~ s100b, type="b", pch=21, col="blue", bg="grey")
aSAH \%>\%
  ntbt_plot.roc(outcome, s100b, type="b", pch=21, col="blue", bg="grey")


## ntbt_roc: Build a ROC curve
## Original function to interface
roc(outcome ~ s100b, data = aSAH, type="b", pch=21, col="blue", bg="grey")
## For non-formula variants, either:
## 1) need to attach
attach(aSAH)
roc(outcome, s100b, type="b", pch=21, col="blue", bg="grey")
detach()
## or use $
roc(aSAH$outcome, aSAH$s100b, type="b", pch=21, col="blue", bg="grey")

## The interface puts data as first parameter
ntbt_roc(aSAH, outcome ~ s100b, type="b", pch=21, col="blue", bg="grey")
ntbt_roc(aSAH, outcome, s100b, type="b", pch=21, col="blue", bg="grey")

## so it can be used easily in a pipeline.
aSAH \%>\%
  ntbt_roc(outcome ~ s100b, type="b", pch=21, col="blue", bg="grey")
aSAH \%>\%
  ntbt_roc(outcome, s100b, type="b", pch=21, col="blue", bg="grey")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{pROC}
\keyword{auc}
\keyword{ci}
\keyword{ci.auc}
\keyword{ci.coords}
\keyword{ci.se}
\keyword{ci.sp}
\keyword{ci.thresholds}
\keyword{multiclass.roc}
\keyword{plot.roc}
\keyword{roc}
