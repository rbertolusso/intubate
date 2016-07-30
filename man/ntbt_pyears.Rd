\name{ntbt_pyears}
\alias{ntbt_pyears}
\title{Interface to pyears for data science pipelines.}
\description{
  \code{ntbt_pyears} is an interface to \code{survival::\link{pyears}}
  (Person Years) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_pyears(data, formula, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{pyears}}.}
}
\details{
  ntbt_pyears reverses the order of \code{data} and \code{formula}, so it can be
  used in a pipeline. It then calls \code{\link{pyears}}.
}
\value{
  Object returned by \code{\link{pyears}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{pyears}}
}
\examples{
library(survival)

hearta <- by(heart, heart$id,  
             function(x)x[x$stop == max(x$stop),]) 
hearta <- do.call("rbind", hearta) 

## Original function to interface
pyears(Surv(stop/365.25, event) ~
         cut(age + 48, c(0,50,60,70,100)) + surgery,
       data = hearta, scale = 1)

## The interface reverses the order of data and formula
ntbt_pyears(data = hearta,
            Surv(stop/365.25, event) ~
              cut(age + 48, c(0,50,60,70,100)) + surgery,
            scale = 1)

## so it can be used easily in a pipeline.
library(magrittr)
hearta \%>\%
  ntbt_pyears(Surv(stop/365.25, event) ~
                cut(age + 48, c(0,50,60,70,100)) + surgery,
              scale = 1)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_pyears}
\keyword{pyears}
