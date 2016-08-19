\name{betareg}
\alias{ntbt_betamix}
\alias{ntbt_betareg}
\alias{ntbt_betatree}
\title{Interfaces for betareg package for data science pipelines.}
\description{
  Interfaces to \code{betareg} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_betamix(data, ...)
ntbt_betareg(data, ...)
ntbt_betatree(data, ...)
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
library(betareg)


## ntbt_betamix: Finite Mixtures of Beta Regression for Rates and Proportions
data("ReadingSkills", package = "betareg")

## Original function to interface
set.seed(4040)
betamix(accuracy ~ iq, data = ReadingSkills, k = 3, nstart = 10,
        extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01))

## The interface puts data as first parameter
set.seed(4040)
ntbt_betamix(ReadingSkills, accuracy ~ iq, k = 3, nstart = 10,
             extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01))

## so it can be used easily in a pipeline.
ReadingSkills \%>\%
  ntbt_betamix(accuracy ~ iq, k = 3, nstart = 10,
               extra_components = extraComponent(type = "uniform", coef = 0.99, delta = 0.01))


## ntbt_betareg: Beta Regression for Rates and Proportions
data("GasolineYield", package = "betareg")

## Original function to interface
betareg(yield ~ batch + temp, data = GasolineYield)

## The interface puts data as first parameter
ntbt_betareg(GasolineYield, yield ~ batch + temp)

## so it can be used easily in a pipeline.
GasolineYield \%>\%
  ntbt_betareg(yield ~ batch + temp)


## ntbt_betatree: Beta Regression Trees
data("ReadingSkills", package = "betareg")
ReadingSkills$x1 <- rnorm(nrow(ReadingSkills))
ReadingSkills$x2 <- runif(nrow(ReadingSkills))
ReadingSkills$x3 <- factor(rnorm(nrow(ReadingSkills)) > 0)

library(partykit)
## Original function to interface
set.seed(1071)
bt <- betatree(accuracy ~ iq | iq, ~ dyslexia + x1 + x2 + x3,
               data = ReadingSkills, minsize = 10)
plot(bt)

## The interface puts data as first parameter
set.seed(1071)
bt <- ntbt_betatree(ReadingSkills, accuracy ~ iq | iq, ~ dyslexia + x1 + x2 + x3,
                    minsize = 10)
plot(bt)

## so it can be used easily in a pipeline.
set.seed(1071)
ReadingSkills \%>\%
  ntbt_betatree(accuracy ~ iq | iq, ~ dyslexia + x1 + x2 + x3, minsize = 10) \%>\%
  plot()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{betareg}
\keyword{betamix}
\keyword{betatree}
