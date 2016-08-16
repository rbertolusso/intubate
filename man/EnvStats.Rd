\name{EnvStats}
%\alias{ntbt_calibrate}
\alias{ntbt_gofTest}
\alias{ntbt_gofGroupTest}
\alias{ntbt_kendallSeasonalTrendTest}
\alias{ntbt_kendallTrendTest}
\alias{ntbt_stripChart}
\alias{ntbt_summaryFull}
\alias{ntbt_summaryStats}
\alias{ntbt_varGroupTest}
\title{Interfaces for EnvStats package for data science pipelines.}
\description{
  Interfaces to \code{EnvStats} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
#ntbt_calibrate(data, ...)   ## Not implement for 0.99.3 as needs an attach()
ntbt_gofTest(data, ...)
ntbt_gofGroupTest(data, ...)
ntbt_kendallSeasonalTrendTest(data, ...)
ntbt_kendallTrendTest(data, ...)
ntbt_stripChart(data, ...)
ntbt_summaryFull(data, ...)
ntbt_summaryStats(data, ...)
ntbt_varGroupTest(data, ...)
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
library(EnvStats)


## ntbt_calibrate: Fit a Calibration Line or Curve
#Cadmium <- EPA.97.cadmium.111.df$Cadmium 
#Spike <- EPA.97.cadmium.111.df$Spike 
data(EPA.97.cadmium.111.df)
## Original function to interface
calibrate(Cadmium ~ Spike, data = EPA.97.cadmium.111.df) 

## NOTE: --check-as-cran does not like having an attach()
##       in the code, which is needed to interface
##       calibrate (at least for now). I will get back
##       to this after 0.99.3 is submitted. Please do not
##       report as bug (instead please find a solution to this!)

## The interface puts data as first parameter
#ntbt_calibrate(EPA.97.cadmium.111.df, Cadmium ~ Spike)

## so it can be used easily in a pipeline.
#EPA.97.cadmium.111.df \%>\%
#  ntbt_calibrate(Cadmium ~ Spike) 


## ntbt_gofGroupTest: Goodness-of-Fit Test for a Specified Probability
##                    Distribution for Groups
## Original function to interface
gofGroupTest(Nickel.ppb ~ Well, data = EPA.09.Ex.10.1.nickel.df)

## The interface puts data as first parameter
ntbt_gofGroupTest(EPA.09.Ex.10.1.nickel.df, Nickel.ppb ~ Well)

## so it can be used easily in a pipeline.
EPA.09.Ex.10.1.nickel.df \%>\%
  ntbt_gofGroupTest(Nickel.ppb ~ Well)

## ntbt_gofTest: Goodness-of-Fit Test
## Original function to interface
gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df)

## The interface puts data as first parameter
ntbt_gofTest(EPA.09.Ex.10.1.nickel.df, Nickel.ppb ~ 1)

## so it can be used easily in a pipeline.
EPA.09.Ex.10.1.nickel.df \%>\%
  ntbt_gofTest(Nickel.ppb ~ 1)


## ntbt_kendallSeasonalTrendTest: Nonparametric Test for Monotonic Trend Within
##                                Each Season Based on Kendall's Tau Statistic
## Original function to interface
kendallSeasonalTrendTest(Unadj.Conc ~ Month + Year, data = EPA.09.Ex.14.8.df)

## The interface puts data as first parameter
ntbt_kendallSeasonalTrendTest(EPA.09.Ex.14.8.df, Unadj.Conc ~ Month + Year)

## so it can be used easily in a pipeline.
EPA.09.Ex.14.8.df \%>\%
  ntbt_kendallSeasonalTrendTest(Unadj.Conc ~ Month + Year)


## ntbt_kendallTrendTest: Kendall's Nonparametric Test for Montonic Trend
## Original function to interface
kendallTrendTest(Sulfate.ppm ~ Sampling.Date, data = EPA.09.Ex.17.6.sulfate.df)

## The interface puts data as first parameter
ntbt_kendallTrendTest(EPA.09.Ex.17.6.sulfate.df, Sulfate.ppm ~ Sampling.Date)

## so it can be used easily in a pipeline.
EPA.09.Ex.17.6.sulfate.df \%>\%
  ntbt_kendallTrendTest(Sulfate.ppm ~ Sampling.Date)


## ntbt_stripChart: 1-D Scatter Plots with Confidence Intervals
## Original function to interface
stripChart(TcCB ~ Area, data = EPA.94b.tccb.df, col = c("red", "blue"), 
           p.value = TRUE, ci.and.test = "nonparametric", 
           ylab = "TcCB (ppb)")

## The interface puts data as first parameter
ntbt_stripChart(EPA.94b.tccb.df, TcCB ~ Area, col = c("red", "blue"), 
                p.value = TRUE, ci.and.test = "nonparametric", 
                ylab = "TcCBntbt_summaryFull (ppb)")

## so it can be used easily in a pipeline.
EPA.94b.tccb.df \%>\%
  ntbt_stripChart(TcCB ~ Area, col = c("red", "blue"), 
                  p.value = TRUE, ci.and.test = "nonparametric", 
                  ylab = "TcCB (ppb)")


## ntbt_summaryFull: Full Complement of Summary Statistics
## Original function to interface
summaryFull(TcCB ~ Area, data = EPA.94b.tccb.df)

## The interface puts data as first parameter
ntbt_summaryFull(EPA.94b.tccb.df, TcCB ~ Area)

## so it can be used easily in a pipeline.
EPA.94b.tccb.df \%>\%
  ntbt_summaryFull(TcCB ~ Area)


## ntbt_summaryStats: Summary Statistics
## Original function to interface
summaryStats(log10(TcCB) ~ Area, data = EPA.94b.tccb.df)

## The interface puts data as first parameter
ntbt_summaryStats(EPA.94b.tccb.df, log10(TcCB) ~ Area)

## so it can be used easily in a pipeline.
EPA.94b.tccb.df \%>\%
  ntbt_summaryStats(log10(TcCB) ~ Area)


## ntbt_varGroupTest: Test for Homogeneity of Variance Among Two or More Groups
## Original function to interface
varGroupTest(Arsenic.ppb ~ Well, data = EPA.09.Ex.11.1.arsenic.df)

## The interface puts data as first parameter
ntbt_varGroupTest(EPA.09.Ex.11.1.arsenic.df, Arsenic.ppb ~ Well)

## so it can be used easily in a pipeline.
EPA.09.Ex.11.1.arsenic.df \%>\%
  ntbt_varGroupTest(Arsenic.ppb ~ Well)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{EnvStats}
\keyword{ntbt_calibrate}
\keyword{ntbt_gofTest}
\keyword{ntbt_gofGroupTest}
\keyword{ntbt_kendallSeasonalTrendTest}
\keyword{ntbt_kendallTrendTest}
\keyword{ntbt_stripChart}
\keyword{ntbt_summaryFull}
\keyword{ntbt_summaryStats}
\keyword{ntbt_varGroupTest}
