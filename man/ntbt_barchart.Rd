\name{ntbt_barchart}
\alias{ntbt_barchart}
\title{Interface to barchart for data science pipelines.}
\description{
  \code{ntbt_barchart} is an interface to \code{lattice::\link{barchart}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_barchart(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{barchart}}.}
}
\details{
  ntbt_barchart reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{barchart}.
}
\value{
  Object returned by \code{\link{barchart}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{barchart}}
}
\examples{
library(lattice)

## Original function to interface
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))

## The interface reverses the order of data and formula
ntbt_barchart(data = barley, yield ~ variety | site,
              groups = year, layout = c(1,6), stack = TRUE,
              auto.key = list(space = "right"),
              ylab = "Barley Yield (bushels/acre)",
              scales = list(x = list(rot = 45)))

## so it can be used easily in a pipeline.
library(magrittr)
barley \%>\%
  ntbt_barchart(yield ~ variety | site,
                groups = year, layout = c(1,6), stack = TRUE,
                auto.key = list(space = "right"),
                ylab = "Barley Yield (bushels/acre)",
                scales = list(x = list(rot = 45)))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_barchart}
\keyword{barchart}
