\name{rminer}
\alias{ntbt_fit}
\alias{ntbt_mining}
\title{Interfaces for rminer package for data science pipelines.}
\description{
  Interfaces to \code{rminer} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_fit(data, ...)
ntbt_mining(data, ...)
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
library(rminer)

## ntbt_fit: Fit a supervised data mining model (classification or regression) model
x1 <- rnorm(200,100,20)
x2 <- rnorm(200,100,20)
y <- 0.7*sin(x1/(25*pi))+0.3*sin(x2/(25*pi))
dta <- data.frame(x1, x2, y)

## Original function to interface
fit(y ~ x1 + x2, data = dta, model = "mlpe")

## The interface puts data as first parameter
ntbt_fit(dta, y ~ x1 + x2, model = "mlpe")

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_fit(y ~ x1 + x2, model = "mlpe")


## ntbt_mining: Powerful function that trains and tests a particular fit model
##              under several runs and a given validation method
## Original function to interface
mining(y ~ x1 + x2, data = dta, model = "mlpe")

## The interface puts data as first parameter
ntbt_mining(dta, y ~ x1 + x2, model = "mlpe")

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_mining(y ~ x1 + x2, model = "mlpe")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{rminer}
\keyword{fit}
\keyword{mining}
