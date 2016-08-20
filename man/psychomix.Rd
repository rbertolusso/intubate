\name{psychomix}
\alias{ntbt_btmix}
\alias{ntbt_raschmix}
\title{Interfaces for psychomix package for data science pipelines.}
\description{
  Interfaces to \code{psychomix} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_btmix(data, ...)
ntbt_raschmix(data, ...)
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
library(psychomix)


## ntbt_btmix: Finite Mixtures of Bradley-Terry Models
data("GermanParties2009", package = "psychotools")

## omit single observation with education = 1
gp <- subset(GermanParties2009, education != "1")
gp$education <- factor(gp$education)

## Original function to interface
set.seed(1)
cm <- btmix(preference ~ gender + education + age + crisis,
            data = gp, k = 1:4, nrep = 3)
plot(cm)

## The interface puts data as first parameter
set.seed(1)
cm <- ntbt_btmix(gp, preference ~ gender + education + age + crisis,
                 k = 1:4, nrep = 3)
plot(cm)

## so it can be used easily in a pipeline.
set.seed(1)
gp \%>\%
  ntbt_btmix(preference ~ gender + education + age + crisis, k = 1:4, nrep = 3) \%>\%
  plot()



## ntbt_raschmix: Finite Mixtures of Rasch Models
set.seed(1)
r2 <- simRaschmix(design = "rost2")
d <- data.frame(
  x1 = rbinom(nrow(r2), prob = c(0.4, 0.6)[attr(r2, "cluster")], size = 1),
  x2 = rnorm(nrow(r2))
)
d$resp <- r2

## Original function to interface
m1 <- raschmix(resp ~ 1, data = d, k = 1:3, score = "saturated")
plot(m1)

## The interface puts data as first parameter
m1 <- ntbt_raschmix(d, resp ~ 1, k = 1:3, score = "saturated")
plot(m1)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_raschmix(resp ~ 1, k = 1:3, score = "saturated") \%>\%
  plot()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{psychomix}
\keyword{btmix}
\keyword{raschmix}
