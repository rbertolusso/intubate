\name{gmnl}
\alias{ntbt_gmnl}
\title{Interfaces for gmnl package for data science pipelines.}
\description{
  Interfaces to \code{gmnl} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gmnl(data, ...)
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
library(gmnl)


## ntbt_gmnl: Estimate Multinomial Logit Models with Observed and
##             Unobserved Individual Heterogeneity
data("TravelMode", package = "AER")
library(mlogit)
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", 
                 alt.levels = c("air", "train", "bus", "car"), chid.var = "individual")

## Original function to interface
gmnl(choice ~ wait + vcost + travel + gcost| 1, data = TM,
     model = "smnl", R = 100,
     notscale = c(1, 1, 1, rep(0, 4)))

## The interface puts data as first parameter
ntbt_gmnl(TM, choice ~ wait + vcost + travel + gcost| 1,
          model = "smnl", R = 100, 
          notscale = c(1, 1, 1, rep(0, 4)))

## so it can be used easily in a pipeline.
TM \%>\%
  ntbt_gmnl(choice ~ wait + vcost + travel + gcost| 1,
            model = "smnl", R = 100, 
            notscale = c(1, 1, 1, rep(0, 4)))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gmnl}
