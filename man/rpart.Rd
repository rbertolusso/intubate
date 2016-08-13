\name{rpart}
\alias{ntbt_rpart}
\title{Interfaces for rpart package for data science pipelines.}
\description{
  Interfaces to \code{rpart} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_rpart(data, ...)
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
library(rpart)

## rpart
## Original function to interface
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(.65,.35), split = "information"))
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)

## The interface reverses the order of data and formula
fit <- ntbt_rpart(data = kyphosis, Kyphosis ~ Age + Number + Start)
fit2 <- ntbt_rpart(data = kyphosis, Kyphosis ~ Age + Number + Start,
                   parms = list(prior = c(.65,.35), split = "information"))

par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)

## so it can be used easily in a pipeline.
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
kyphosis \%>\%
  ntbt_rpart(Kyphosis ~ Age + Number + Start) \%T>\%
  plot() \%>\%
  text(use.n = TRUE)
kyphosis \%>\%
  ntbt_rpart(Kyphosis ~ Age + Number + Start,
             parms = list(prior = c(.65,.35), split = "information")) \%T>\%
  plot() \%>\%
  text(use.n = TRUE)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{rpart}
