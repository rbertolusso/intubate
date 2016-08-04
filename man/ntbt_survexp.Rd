\name{ntbt_survexp}
\alias{ntbt_survexp}
\title{Interface to survexp for data science pipelines.}
\description{
  \code{ntbt_survexp} is an interface to \code{survival::\link{survexp}}
  (Compute Expected Survival) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_survexp(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{survexp}}.}
}
\details{
  ntbt_survexp calls \code{\link{survexp}}.
}
\value{
  Object returned by \code{\link{survexp}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{survexp}}
}
\examples{
\dontrun{
library(survival)

## Original function to interface
fit1 <- survexp(futime ~ 1,data=jasa, 
                rmap=list(sex="male", year=accept.dt,   
                          age=(accept.dt-birth.dt)),
                method='conditional')
summary(fit1, times=1:10*182.5, scale=365)

## The interface reverses the order of data and formula
fit1 <- ntbt_survexp(data=jasa, futime ~ 1,
                  rmap=list(sex="male", year=accept.dt,   
                            age=(accept.dt-birth.dt)),
                  method='conditional')
summary(fit1, times=1:10*182.5, scale=365)

## so it can be used easily in a pipeline.
library(magrittr)
jasa \%>\%
  ntbt_survexp(futime ~ 1,
            rmap=list(sex="male", year=accept.dt,   
                      age=(accept.dt-birth.dt)),
            method='conditional') \%>\%
  summary(times=1:10*182.5, scale=365)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_survexp}
\keyword{survexp}
