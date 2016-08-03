\name{ntbt_nlme}
\alias{ntbt_nlme}
\title{Interface to nlme for data science pipelines.}
\description{
  \code{ntbt_nlme} is an interface to \code{nlme::\link{nlme}}
  (Nonlinear Mixed-Effects Models) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_nlme(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{nlme}}.}
}
\details{
  ntbt_nlme calls \code{nlme}.
}
\value{
  Object returned by \code{\link{nlme}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{nlme}}
}
\examples{
library(nlme)

## Original function to interface
fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(fm1)

## The interface reverses the order of data and formula
fm1 <- ntbt_nlme(data = Loblolly,
                 height ~ SSasymp(age, Asym, R0, lrc),
                 fixed = Asym + R0 + lrc ~ 1,
                 random = Asym ~ 1,
                 start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(fm1)

## so it can be used easily in a pipeline.
library(magrittr)
Loblolly \%>\%
  ntbt_nlme(height ~ SSasymp(age, Asym, R0, lrc),
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3)) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_nlme}
\keyword{nlme}
