\name{gam}
\alias{ntbt_gam}
\title{Interfaces for gam package for data science pipelines.}
\description{
  Interfaces to \code{gam} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gam(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list, ...}
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
\seealso{
  \code{gam}
}
\examples{
\dontrun{
library(gam)
data(kyphosis)

## Original function to interface
fit <- gam(Kyphosis ~ s(Age,4) + Number, family = binomial, data = kyphosis,
    trace = TRUE)
summary(fit)
fit <- gam(Kyphosis ~ poly(Age,2) + s(Start), data = kyphosis,
           family = binomial, subset = Number > 2)
summary(fit)
fit <- gam(Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp),
           data = airquality, na = na.gam.replace)
summary(fit)

## The interface reverses the order of data and formula
fit <- ntbt_gam(kyphosis, Kyphosis ~ s(Age,4) + Number,
                family = binomial, trace = TRUE)
summary(fit)
fit <- ntbt_gam(data = kyphosis, Kyphosis ~ poly(Age,2) + s(Start),
                family = binomial, subset = Number > 2)
summary(fit)
fit <- ntbt_gam(data = airquality, Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp),
                na = na.gam.replace)
summary(fit)

## so it can be used easily in a pipeline.
library(magrittr)
kyphosis \%>\%
  ntbt_gam(Kyphosis ~ s(Age,4) + Number,
          family = binomial, trace = TRUE) \%>\%
  summary()

kyphosis \%>\%
  ntbt_gam(Kyphosis ~ poly(Age,2) + s(Start),
           family = binomial, subset = Number > 2) \%>\%
  summary()
  
airquality \%>\%
  ntbt_gam(Ozone^(1/3) ~ lo(Solar.R) + lo(Wind, Temp),
           na = na.gam.replace) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gam}
