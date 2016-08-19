\name{flexmix}
\alias{ntbt_flexmix}
\alias{ntbt_initFlexmix}
\alias{ntbt_stepFlexmix}
\title{Interfaces for flexmix package for data science pipelines.}
\description{
  Interfaces to \code{flexmix} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_flexmix(data, ...)
ntbt_initFlexmix(data, ...)
ntbt_stepFlexmix(data, ...)
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
library(flexmix)


## ntbt_flexmix: Flexible Mixture Modeling
data("NPreg", package = "flexmix")

## Original function to interface
set.seed(1)
ex1 <- flexmix(yn ~ x + I(x^2), data = NPreg, k = 2,
               control = list(verb = 5, iter = 100))
plot(ex1)

## The interface puts data as first parameter
set.seed(1)
ex1 <- ntbt_flexmix(NPreg, yn ~ x + I(x^2), k = 2,
                    control = list(verb = 5, iter = 100))
plot(ex1)

## so it can be used easily in a pipeline.
set.seed(1)
NPreg \%>\%
  ntbt_flexmix(yn ~ x + I(x^2), k = 2,
               control = list(verb = 5, iter = 100)) \%>\%
  plot()

## NOTE: it seems we could also add initFlexmix and stepFlexmix,
##       that call flexmix repeteadly. I will not include examples
##       for those for now.
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{flexmix}
\keyword{initFlexmix}
\keyword{stepFlexmix}
