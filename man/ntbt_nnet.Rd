\name{ntbt_nnet}
\alias{ntbt_nnet}
\title{Interface to nnet for data science pipelines.}
\description{
  \code{ntbt_nnet} is an interface to \code{nnet::\link{nnet}}
  (Fit Neural Networks) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_nnet(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{nnet}}.}
}
\details{
  ntbt_nnet reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{nnet}.
}
\value{
  Object returned by nnet function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{nnet}}
}
\examples{
library(nnet)

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
library(magrittr)
set.seed(12345) ## for reproducible results
ird \%>\%
  ntbt_nnet(species ~ ., subset = samp,
            size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_nnet}
\keyword{nnet}
