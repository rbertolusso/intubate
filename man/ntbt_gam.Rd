\name{ntbt_gam}
\alias{ntbt_gam}
\title{Interface to gam for data science pipelines.}
\description{
  \code{ntbt_gam} is an interface to \code{gam::\link{gam}}
  (Fitting Generalized Additive Models) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_gam(data, formula, family=gaussian, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{gam}}.}
}
\details{
  ntbt_gam calls \code{\link{gam}}.
}
\value{
  Object returned by \code{\link{gam}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{gam}}
}
\examples{
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
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_gam}
\keyword{gam}
