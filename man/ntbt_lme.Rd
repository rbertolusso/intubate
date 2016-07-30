\name{ntbt_lme}
\alias{ntbt_lme}
\title{Interface to lme for data science pipelines.}
\description{
  \code{ntbt_lme} is an interface to \code{nlme::\link{lme}}
  (Linear Mixed-Effects Models) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_lme(data, fixed, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{fixed}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{lme}}.}
}
\details{
  ntbt_lme reverses the order of \code{data} and \code{fixed}, so it can be
  used in a pipeline. It then calls \code{lme}.
}
\value{
  Object returned by \code{\link{lme}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{lme}}
}
\examples{
library(nlme)

## Original function to interface
lme(distance ~ age, data = Orthodont) # random is ~ age
lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)

## The interface reverses the order of data and formula
ntbt_lme(data = Orthodont, distance ~ age) # random is ~ age
ntbt_lme(data = Orthodont, distance ~ age + Sex, random = ~ 1)

## so it can be used easily in a pipeline.
library(magrittr)
Orthodont \%>\%
  ntbt_lme(distance ~ age) # random is ~ age
Orthodont \%>\%
  ntbt_lme(distance ~ age + Sex, random = ~ 1)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_lme}
\keyword{lme}
