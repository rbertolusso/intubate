\name{nlme}
\alias{ntbt_gls}
\alias{ntbt_lme}
\alias{ntbt_lmList}
\alias{ntbt_nlme}
\alias{ntbt_nlsList}
\title{Interfaces for nlme package for data science pipelines.}
\description{
  Interfaces to \code{nlme} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gls(data, ...)
ntbt_lme(data, ...)
ntbt_lmList(data, ...)
ntbt_nlme(data, ...)
ntbt_nlsList(data, ...)
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
library(magrittr)
library(nlme)

## gls
## Original function to interface
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
summary(fm1)

## The interface reverses the order of data and formula
fm1 <- ntbt_gls(Ovary, follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                correlation = corAR1(form = ~ 1 | Mare))
summary(fm1)

## so it can be used easily in a pipeline.
Ovary \%>\%
  ntbt_gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                correlation = corAR1(form = ~ 1 | Mare))  \%>\%
  summary()
  
## nlme
## Original function to interface
lme(distance ~ age, data = Orthodont) # random is ~ age
lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)

## The interface reverses the order of data and formula
ntbt_lme(data = Orthodont, distance ~ age) # random is ~ age
ntbt_lme(data = Orthodont, distance ~ age + Sex, random = ~ 1)

## so it can be used easily in a pipeline.
Orthodont \%>\%
  ntbt_lme(distance ~ age) # random is ~ age
Orthodont \%>\%
  ntbt_lme(distance ~ age + Sex, random = ~ 1)

## lmList
## Original function to interface
lmList(distance ~ age | Subject, Orthodont)

## The interface reverses the order of data and formula
ntbt_lmList(Orthodont, distance ~ age | Subject)

## so it can be used easily in a pipeline.
Orthodont \%>\%
  ntbt_lmList(distance ~ age | Subject)
  
Orthodont \%>\%
  ntbt_lmList(distance ~ age | Subject) \%>\%
  summary()

## nlme
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
Loblolly \%>\%
  ntbt_nlme(height ~ SSasymp(age, Asym, R0, lrc),
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3)) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{gls}
\keyword{lme}
\keyword{lmList}
\keyword{nlme}
\keyword{nlsList}
