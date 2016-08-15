\name{MCMCglmm}
\alias{ntbt_MCMCglmm}
\title{Interfaces for MCMCglmm package for data science pipelines.}
\description{
  Interfaces to \code{MCMCglmm} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_MCMCglmm(data, ...)
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
library(MCMCglmm)


## ntbt_MCMCglmm: Multivariate Generalised Linear Mixed Models
data(PlodiaPO)

## Original function to interface
set.seed(1)
model <- MCMCglmm(PO ~ 1, random = ~ FSfamily, data = PlodiaPO, verbose = FALSE)
summary(model)

## The interface puts data as first parameter
set.seed(1)
model <- ntbt_MCMCglmm(PlodiaPO, PO ~ 1, random = ~ FSfamily, verbose = FALSE)
summary(model)

## so it can be used easily in a pipeline.
set.seed(1)
PlodiaPO \%>\%
  ntbt_MCMCglmm(PO ~ 1, random = ~ FSfamily, verbose = FALSE) %>%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{MCMCglmm}
