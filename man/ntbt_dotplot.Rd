\name{ntbt_dotplot}
\alias{ntbt_dotplot}
\title{Interface to dotplot for data science pipelines.}
\description{
  \code{ntbt_dotplot} is an interface to \code{lattice::\link{dotplot}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_dotplot(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{dotplot}}.}
}
\details{
  ntbt_dotplot reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{dotplot}.
}
\value{
  Object returned by \code{\link{dotplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{dotplot}}
}
\examples{
library(lattice)

## Original function to interface
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)

## The interface reverses the order of data and formula
ntbt_dotplot(data = barley, variety ~ yield | site, groups = year,
             key = simpleKey(levels(barley$year), space = "right"),
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)

## so it can be used easily in a pipeline.
library(magrittr)
barley \%>\%
  ntbt_dotplot(variety ~ yield | site, groups = year,
             key = simpleKey(levels(barley$year), space = "right"),
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_dotplot}
\keyword{dotplot}
