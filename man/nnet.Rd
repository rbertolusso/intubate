\name{nnet}
\alias{ntbt_multinom}
\alias{ntbt_nnet}
\title{Interfaces for nnet package for data science pipelines.}
\description{
  Interfaces to \code{nnet} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_multinom(data, ...)
ntbt_nnet(data, ...)
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
library(nnet)

## multinom
options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)

## Original function to interface
multinom(low ~ ., bwt)

## The interface reverses the order of data and formula
ntbt_multinom(bwt, low ~ .)

## so it can be used easily in a pipeline.
bwt \%>\%
  ntbt_multinom(low ~ .)

## nnet
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)))
set.seed(6789) ## for reproducible results
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                  species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))

## Original function to interface
set.seed(12345) ## for reproducible results
nnet(species ~ ., data = ird, subset = samp,
     size = 2, rang = 0.1, decay = 5e-4, maxit = 200)

## The interface reverses the order of data and formula
set.seed(12345) ## for reproducible results
ntbt_nnet(data = ird, species ~ ., subset = samp,
          size = 2, rang = 0.1, decay = 5e-4, maxit = 200)

## so it can be used easily in a pipeline.
set.seed(12345) ## for reproducible results
ird \%>\%
  ntbt_nnet(species ~ ., subset = samp,
            size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{nnet}
\keyword{multinom}
