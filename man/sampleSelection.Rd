\name{sampleSelection}
\alias{ntbt_probit}
\alias{ntbt_binaryChoice}
\alias{ntbt_selection}
\alias{ntbt_heckit}
\title{Interfaces for sampleSelection package for data science pipelines.}
\description{
  Interfaces to \code{sampleSelection} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_probit(data, ...)
ntbt_binaryChoice(data, ...)
ntbt_selection(data, ...)
ntbt_heckit(data, ...)
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
library(sampleSelection)


## ntbt_probit: Binary choice models
## ntbt_binaryChoice:  (no examples found)
data(Mroz87)
Mroz87$kids <- Mroz87$kids5 > 0 | Mroz87$kids618 > 0
Mroz87$age30.39 <- Mroz87$age < 40
Mroz87$age50.60 <- Mroz87$age >= 50

## Original function to interface
probit(lfp ~ kids + age30.39 + age50.60 + educ + hushrs +
         huseduc + huswage + mtr + motheduc, data=Mroz87)

## The interface puts data as first parameter
ntbt_probit(Mroz87, lfp ~ kids + age30.39 + age50.60 + educ + hushrs +
              huseduc + huswage + mtr + motheduc)

## so it can be used easily in a pipeline.
Mroz87 \%>\%
  ntbt_probit(lfp ~ kids + age30.39 + age50.60 + educ + hushrs +
                huseduc + huswage + mtr + motheduc)



## ntbt_selection: Heckman-style selection models
## ntbt_heckit:
data( Mroz87 )
Mroz87$kids  <- ( Mroz87$kids5 + Mroz87$kids618 > 0 )

## Original function to interface
# Two-step estimation
heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
        wage ~ exper + I( exper^2 ) + educ + city, Mroz87)
# ML estimation
selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
          wage ~ exper + I( exper^2 ) + educ + city, Mroz87)

## The interface puts data as first parameter
# Two-step estimation
ntbt_heckit(Mroz87, lfp ~ age + I( age^2 ) + faminc + kids + educ,
            wage ~ exper + I( exper^2 ) + educ + city)
# ML estimation
ntbt_selection(Mroz87, lfp ~ age + I( age^2 ) + faminc + kids + educ,
               wage ~ exper + I( exper^2 ) + educ + city)

## so it can be used easily in a pipeline.
# Two-step estimation
Mroz87 \%>\%
  ntbt_heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
              wage ~ exper + I( exper^2 ) + educ + city)
# ML estimation
Mroz87 \%>\%
  ntbt_selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                 wage ~ exper + I( exper^2 ) + educ + city)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{sampleSelection}
\keyword{probit}
\keyword{binaryChoice}
\keyword{selection}
\keyword{heckit}
