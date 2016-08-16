\name{mlogit}
\alias{ntbt_mlogit}
\title{Interfaces for mlogit package for data science pipelines.}
\description{
  Interfaces to \code{mlogit} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_mlogit(data, ...)
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
library(mlogit)


## ntbt_mlogit: Multinomial logit model
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

## Original function to interface
mlogit(mode ~ price + catch, data = Fish)

## The interface puts data as first parameter
ntbt_mlogit(Fish, mode ~ price + catch)

## so it can be used easily in a pipeline.
Fish \%>\%
  ntbt_mlogit(mode ~ price + catch)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{mlogit}
