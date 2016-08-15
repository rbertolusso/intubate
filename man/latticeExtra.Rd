\name{latticeExtra}
\alias{ntbt_ecdfplot}
\alias{ntbt_mapplot}
\alias{ntbt_rootogram}
\alias{ntbt_segplot}
\alias{ntbt_tileplot}
\title{Interfaces for latticeExtra package for data science pipelines.}
\description{
  Interfaces to \code{latticeExtra} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_ecdfplot(data, ...)
ntbt_mapplot(data, ...)
ntbt_rootogram(data, ...)
ntbt_segplot(data, ...)
ntbt_tileplot(data, ...)
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
library(latticeExtra)


## ntbt_ecdfplot: Trellis Displays of Empirical CDF
data(singer, package = "lattice")

## Original function to interface
ecdfplot(~height | voice.part, data = singer)

## The interface puts data as first parameter
ntbt_ecdfplot(singer, ~height | voice.part)

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_ecdfplot(~height | voice.part)


## ntbt_mapplot: Trellis displays on Maps a.k.a. Choropleth maps
library(maps)
library(mapproj)
data(USCancerRates)

## Original function to interface
## Note: Alaska, Hawaii and others are not included in county map;
## this generates warnings with both USCancerRates and ancestry.
suppressWarnings(print(
  
  mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
          data = USCancerRates,
          map = map("county", plot = FALSE, fill = TRUE,
                    projection = "mercator"))
  
))

## The interface puts data as first parameter
suppressWarnings(print(
  
  ntbt_mapplot(USCancerRates, rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
               map = map("county", plot = FALSE, fill = TRUE,
                         projection = "mercator"))

  ))

## so it can be used easily in a pipeline.
suppressWarnings(print(
  
  USCancerRates \%>\%
  ntbt_mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
               map = map("county", plot = FALSE, fill = TRUE,
                         projection = "mercator"))

  ))


## ntbt_rootogram: Trellis Displays of Tukey's Hanging Rootograms
library(lattice)
dta <- data.frame(x = rpois(1000, lambda = 50))

## Original function to interface
rootogram(~x, data = dta, dfun = function(x) dpois(x, lambda = 50))

## The interface puts data as first parameter
ntbt_rootogram(dta, ~x, dfun = function(x) dpois(x, lambda = 50))

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_rootogram(~x, dfun = function(x) dpois(x, lambda = 50))


## ntbt_segplot: Plot segments using the Trellis framework
data(USCancerRates)

## Original function to interface
segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male,
        data = subset(USCancerRates, state == "Washington"))

## The interface puts data as first parameter
ntbt_segplot(subset(USCancerRates, state == "Washington"),
             reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male)

## so it can be used easily in a pipeline.
subset(USCancerRates, state == "Washington") \%>\%
  ntbt_segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male)

USCancerRates \%>\%
  subset(state == "Washington") \%>\%
  ntbt_segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male)


## ntbt_tileplot: Plot a spatial mosaic from irregular 2D points
tmp <- state.center
tmp$Income <- state.x77[,"Income"]
library(deldir)

## Original function to interface
tileplot(Income ~ x * y, tmp, border = "black",
         panel = function(x, y, ...) {
           panel.voronoi(x, y, ..., points = FALSE)
           panel.text(x, y, state.abb, cex = 0.6)
         })

## The interface puts data as first parameter
ntbt_tileplot(tmp, Income ~ x * y, border = "black",
              panel = function(x, y, ...) {
                panel.voronoi(x, y, ..., points = FALSE)
                panel.text(x, y, state.abb, cex = 0.6)
              })

## so it can be used easily in a pipeline.
tmp \%>\%
  ntbt_tileplot(Income ~ x * y, border = "black",
                panel = function(x, y, ...) {
                  panel.voronoi(x, y, ..., points = FALSE)
                  panel.text(x, y, state.abb, cex = 0.6)
                })
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{latticeExtra}
\keyword{ecdfplot}
\keyword{mapplot}
\keyword{rootogram}
\keyword{segplot}
\keyword{tileplot}
