\name{ntbt_gls}
\alias{ntbt_gls}
\title{Interface to gls for data science pipelines.}
\description{
  \code{ntbt_gls} is an interface to \code{nlme::\link{gls}}
  (Fit Linear Model Using Generalized Least Squares) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_gls(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{gls}}.}
}
\details{
  ntbt_gls calls \code{gls}.
}
\value{
  Object returned by \code{\link{gls}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{gls}}
}
\examples{
library(nlme)

## Original function to interface
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
summary(fm1)

## The interface reverses the order of data and formula
fm1 <- ntbt_gls(Ovary, follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                correlation = corAR1(form = ~ 1 | Mare))
summary(fm1)

## so it can be used easily in a pipeline.
library(magrittr)
Ovary \%>\%
  ntbt_gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                correlation = corAR1(form = ~ 1 | Mare))  \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_gls}
\keyword{gls}
