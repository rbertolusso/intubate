\name{kernlab}
\alias{ntbt_gausspr}
\alias{ntbt_kfa}
\alias{ntbt_kha}
\alias{ntbt_kkmeans}
\alias{ntbt_kpca}
\alias{ntbt_kqr}
\alias{ntbt_ksvm}
\alias{ntbt_lssvm}
\alias{ntbt_rvm}
\alias{ntbt_sigest}
\alias{ntbt_specc}
\title{Interfaces for kernlab package for data science pipelines.}
\description{
  Interfaces to \code{kernlab} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gausspr(data, ...)
ntbt_kfa(data, ...)
ntbt_kha(data, ...)
ntbt_kkmeans(data, ...)
ntbt_kpca(data, ...)
ntbt_kqr(data, ...)
ntbt_ksvm(data, ...)
ntbt_lssvm(data, ...)
ntbt_rvm(data, ...)
ntbt_sigest(data, ...)
ntbt_specc(data, ...)
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
library(kernlab)

## NOTE: For some reason (unknown to me), when I check the whole intubate
##       package, some of the original functions in kernlab fail with error:
## Segmentation fault (core dumped) when run.
##       However, if I run all the examples below in a fresh session,
##       there is no error. Just in case I will comment everything for now
##       (so I can "R CMD check" without errors).
##       The interfaces seem to work fine. The errors happen when
##       the original functions are running, after being called
##       by the interfaces (again, only when checking the whole examples).
##       Maybe there is some interference with other packages.
##       If you know what may be going on please let me know.

##  ntbt_gausspr: Gaussian processes for regression and classification
##data(iris)

## Original function to interface
##gausspr(Species ~ ., data = iris, var = 2)

## The interface puts data as first parameter
##ntbt_gausspr(iris, Species ~ ., var = 2)

## so it can be used easily in a pipeline.
##iris \%>\%
##  ntbt_gausspr(Species ~ ., var = 2)


## ntbt_kfa: Kernel Feature Analysis
##data(promotergene)

## Original function to interface
##kfa(~ ., data = promotergene)

## The interface puts data as first parameter
##ntbt_kfa(promotergene, ~ .)

## so it can be used easily in a pipeline.
##promotergene \%>\%
##  ntbt_kfa(~ .)


## ntbt_kha: Kernel Principal Components Analysis
##data(iris)
##test <- sample(1:150,70)

## Original function to interface
##kpc <- kha(~ ., data = iris[-test, -5], kernel = "rbfdot", kpar = list(sigma=0.2),
##           features = 2, eta = 0.001, maxiter = 65)
##pcv(kpc)

## The interface puts data as first parameter
##kpc <- ntbt_kha(iris[-test, -5], ~ ., kernel = "rbfdot", kpar = list(sigma=0.2),
##                features = 2, eta = 0.001, maxiter = 65)
##pcv(kpc)

## so it can be used easily in a pipeline.
##iris[-test, -5] \%>\%
##  ntbt_kha(~ ., kernel = "rbfdot", kpar = list(sigma=0.2),
##           features = 2, eta = 0.001, maxiter = 65) \%>\%
##  pcv()


## ntbt_kkmeans: Kernel k-means
## Original function to interface
##sc <- kkmeans(~ ., data = iris[-test, -5], centers = 3)
##centers(sc)

## The interface puts data as first parameter
##sc <- ntbt_kkmeans(iris[-test, -5], ~ ., centers = 3)
##centers(sc)

## so it can be used easily in a pipeline.
##iris[-test, -5] \%>\%
##  ntbt_kkmeans(~ ., centers = 3) \%>\%
##  centers()


## ntbt_kpca: Kernel Principal Components Analysis
##data(iris)
##test <- sample(1:150,20)

## Original function to interface
##kpc <- kpca(~ ., data = iris[-test, -5], kernel = "rbfdot",
##            kpar = list(sigma = 0.2), features = 2)
##pcv(kpc)

## The interface puts data as first parameter
##kpc <- ntbt_kpca(iris[-test, -5], ~ ., kernel = "rbfdot",
##                 kpar = list(sigma = 0.2), features = 2)
##pcv(kpc)

## so it can be used easily in a pipeline.
##iris[-test, -5] \%>\%
##  ntbt_kpca(~ ., kernel = "rbfdot",
##            kpar = list(sigma = 0.2), features = 2) \%>\%
##  pcv()


## ntbt_kqr: Kernel Quantile Regression
## Not found example using formula interface, and I am
## completely ignorant to construct one.
##x <- sort(runif(300))
##y <- sin(pi*x) + rnorm(300,0,sd=exp(sin(2*pi*x)))

##dkqr <- data.frame(x, y)

## Original function to interface
##set.seed(1)
## There is a:
## Segmentation fault (core dumped) when run. Commented for now.
##kqr(x, y, tau = 0.5, C = 0.15)

## The interface puts data as first parameter
##set.seed(1)
##ntbt_kqr(dkqr, x, y, tau = 0.5, C = 0.15)

## so it can be used easily in a pipeline.
##set.seed(1)
##dkqr \%>\%
  ## ntbt_kqr(x, y, tau = 0.5, C = 0.15)


## ntbt_ksvm: Support Vector Machines
##data(spam)
##index <- sample(1:dim(spam)[1])
##spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
##spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

## Original function to interface
##set.seed(1)
##ksvm(type ~ ., data = spamtrain, kernel = "rbfdot",
##     kpar = list(sigma = 0.05), C = 5, cross = 3)

## The interface puts data as first parameter
##set.seed(1)
##ntbt_ksvm(spamtrain, type ~ ., kernel = "rbfdot",
##          kpar = list(sigma = 0.05), C = 5, cross = 3)

## so it can be used easily in a pipeline.
##set.seed(1)
##spamtrain \%>\%
##  ntbt_ksvm(type ~ ., kernel = "rbfdot",
##            kpar = list(sigma = 0.05), C = 5, cross = 3)


## ntbt_lssvm: Least Squares Support Vector Machine
##data(iris)

## Original function to interface
##set.seed(1)
## There is a:
## Segmentation fault (core dumped) when run. Commented for now.
## lssvm(Species ~ ., data = iris)

## The interface puts data as first parameter
##set.seed(1)
##ntbt_lssvm(iris, Species ~ .)

## so it can be used easily in a pipeline.
##set.seed(1)
##iris \%>\%
##  ntbt_lssvm(Species ~ .)


## ntbt_rvm: Relevance Vector Machine
## Not found example using formula interface, and I am
## completely ignorant to construct one.
##x <- seq(-20,20,0.1)
##y <- sin(x)/x + rnorm(401,sd=0.05)

##drvm <- data.frame(x, y)

## Original function to interface
##set.seed(1)
##rvm(x, y, tau = 0.5, C = 0.15)

## The interface puts data as first parameter
##set.seed(1)
##ntbt_rvm(drvm, x, y, tau = 0.5, C = 0.15)

## so it can be used easily in a pipeline.
##set.seed(1)
##drvm \%>\%
##  ntbt_rvm(x, y, tau = 0.5, C = 0.15)


## ntbt_sigest: Hyperparameter estimation for the Gaussian Radial Basis kernel
##data(promotergene)

## Original function to interface
##set.seed(1)
##sigest(Class ~ ., data = promotergene)

## The interface puts data as first parameter
##set.seed(1)
##ntbt_sigest(promotergene, Class ~ .)

## so it can be used easily in a pipeline.
##set.seed(1)
##promotergene \%>\%
##  ntbt_sigest(Class ~ .)

## ntbt_specc: Spectral Clustering
## Not found example using formula interface, and I am
## completely ignorant to construct one.
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{kernlab}
\keyword{gausspr}
\keyword{kfa}
\keyword{kha}
\keyword{kkmeans}
\keyword{kpca}
\keyword{kqr}
\keyword{ksvm}
\keyword{lssvm}
\keyword{rvm}
\keyword{sigest}
\keyword{specc}
