\name{kknn}
\alias{ntbt_train.kknn}
\alias{ntbt_cv.kknn}
\alias{ntbt_kknn}
\title{Interfaces for kknn package for data science pipelines.}
\description{
  Interfaces to \code{kknn} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_train.kknn(data, ...)
ntbt_cv.kknn(data, ...)
ntbt_kknn(data, ...)
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
library(kknn)


## ntbt_train.kknn: Training kknn
## ntbt_cv.kknn:
data(miete)

## Original function to interface
train.kknn(nmqm ~ wfl + bjkat + zh, data = miete,
           kmax = 25, kernel = c("rectangular", "triangular", "epanechnikov",
                                 "gaussian", "rank", "optimal"))
cv.kknn(nmqm ~ wfl + bjkat + zh, data = miete)

## The interface puts data as first parameter
ntbt_train.kknn(miete, nmqm ~ wfl + bjkat + zh,
                kmax = 25, kernel = c("rectangular", "triangular", "epanechnikov",
                                      "gaussian", "rank", "optimal"))
ntbt_cv.kknn(miete, nmqm ~ wfl + bjkat + zh)

## so it can be used easily in a pipeline.
miete \%>\%
  ntbt_train.kknn(nmqm ~ wfl + bjkat + zh,
                  kmax = 25, kernel = c("rectangular", "triangular", "epanechnikov",
                                        "gaussian", "rank", "optimal"))
miete \%>\%
  ntbt_cv.kknn(nmqm ~ wfl + bjkat + zh)

## ntbt_kknn: Weighted k-Nearest Neighbor Classifier
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]

## Original function to interface
kknn(Species ~ ., iris.learn, iris.valid, distance = 1, kernel = "triangular")

## The interface puts data as first parameter
ntbt_kknn(iris.learn, Species ~ ., iris.valid, distance = 1, kernel = "triangular")

## so it can be used easily in a pipeline.
iris.learn \%>\%
  ntbt_kknn(Species ~ ., iris.valid, distance = 1, kernel = "triangular")
## NOTE: there is (in your face) cheating! We should be able to supply
##       both iris.learn and iris.valid. It should be possible with intuBags.
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{kknn}
\keyword{train.kknn}
\keyword{cv.kknn}
