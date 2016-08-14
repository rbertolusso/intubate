\name{brglm}
\alias{ntbt_brglm}
\title{Interfaces for brglm package for data science pipelines.}
\description{
  Interfaces to \code{brglm} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_brglm(data, ...)
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
library(brglm)

## ntbt_brglm: Bias reduction in Binomial-response GLMs
data(lizards)

## Original function to interface
brglm(cbind(grahami, opalinus) ~ height + diameter +
        light + time, family = binomial(logit), data = lizards,
      method = "brglm.fit")
## The interface puts data as first parameter
ntbt_brglm(lizards, cbind(grahami, opalinus) ~ height + diameter +
             light + time, family = binomial(logit),
           method = "brglm.fit")

## so it can be used easily in a pipeline.
lizards \%>\%
  ntbt_brglm(cbind(grahami, opalinus) ~ height + diameter +
               light + time, family = binomial(logit),
             method = "brglm.fit")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{brglm}
