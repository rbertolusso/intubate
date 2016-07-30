\name{ntbt_rpart}
\alias{ntbt_rpart}
\title{Interface to rpart for data science pipelines.}
\description{
  \code{ntbt_rpart} is an interface to \code{rpart::\link{rpart}}
  (Recursive Partitioning and Regression Trees) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_rpart(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{rpart}}.}
}
\details{
  ntbt_rpart reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{rpart}.
}
\value{
  Object returned by rpart function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{rpart}}
}
\examples{
library(rpart)

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
library(magrittr)
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
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_rpart}
\keyword{rpart}
