\name{gplots}
\alias{ntbt_bandplot}
\alias{ntbt_lowess}
\alias{ntbt_overplot}
\alias{ntbt_plotmeans}
\title{Interfaces for gplots package for data science pipelines.}
\description{
  Interfaces to \code{gplots} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bandplot(data, ...)
ntbt_lowess(data, ...)
ntbt_overplot(data, ...)
ntbt_plotmeans(data, ...)
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
library(gplots)


## ntbt_bandplot: Plot x-y Points with Locally Smoothed Mean and Standard Deviation
x <- 1:1000
y <- rnorm(1000, mean=1, sd=1 + x/1000 )
dta <- data.frame(x, y)
rm(x, y)

## Original function to interface
bandplot(y ~ x, data = dta)

## The interface puts data as first parameter
ntbt_bandplot(dta, y ~ x)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_bandplot(y ~ x)


## ntbt_lowess: Scatter Plot Smoothing
## Original function to interface
lowess(dist ~ speed, data = cars)

## The interface puts data as first parameter
ntbt_lowess(cars, dist ~ speed)

## so it can be used easily in a pipeline.
cars \%>\%
  ntbt_lowess(dist ~ speed)

cars \%>\%
  ntbt_plot(dist ~ speed, main="lowess(cars)") \%>\%
  ntbt_lowess(dist ~ speed) \%>\%
  lines(col=2, lty=2)


## ntbt_overplot: Plot multiple variables on the same region,
##                with appropriate axes
data(rtPCR)

## Original function to interface
overplot(RQ ~ Conc..ug.ml. | Test.Substance,
         data = rtPCR,
         subset = Detector == "ProbeType 1" & Conc..ug.ml. > 0,
         same.scale = TRUE,
         log="xy",
         f=3/4,
         main="Detector=ProbeType 1",
         xlab="Concentration (ug/ml)",
         ylab="Relative Gene Quantification"
)## Original function to interface

## The interface puts data as first parameter
ntbt_overplot(rtPCR,
              RQ ~ Conc..ug.ml. | Test.Substance,
              subset = Detector == "ProbeType 1" & Conc..ug.ml. > 0,
              same.scale = TRUE,
              log="xy",
              f=3/4,
              main="Detector=ProbeType 1",
              xlab="Concentration (ug/ml)",
              ylab="Relative Gene Quantification"
)## Original function to interface

## so it can be used easily in a pipeline.
rtPCR \%>\%
  ntbt_overplot(RQ ~ Conc..ug.ml. | Test.Substance,
                subset = Detector == "ProbeType 1" & Conc..ug.ml. > 0,
                same.scale = TRUE,
                log="xy",
                f=3/4,
                main="Detector=ProbeType 1",
                xlab="Concentration (ug/ml)",
                ylab="Relative Gene Quantification"
  )## Original function to interface


## ntbt_plotmeans: Plot Group Means and Confidence Intervals
data(state)
dta <- data.frame(state.abb, state.region)

## Original function to interface
plotmeans(state.area ~ state.region, data = dta)

## The interface puts data as first parameter
ntbt_plotmeans(dta, state.area ~ state.region)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_plotmeans(state.area ~ state.region)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gplots}
\keyword{bandplot}
\keyword{lowess}
\keyword{overplot}
\keyword{plotmeans}
