\name{ntbt_splom}
\alias{ntbt_splom}
\title{Interface to splom for data science pipelines.}
\description{
  \code{ntbt_splom} is an interface to \code{lattice::\link{splom}}
  (Common Bivariate Trellis Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_splom(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{splom}}.}
}
\details{
  ntbt_splom calls \code{splom}.
}
\value{
  Object returned by \code{\link{splom}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{splom}}
}
\examples{
library(lattice)
super.sym <- trellis.par.get("superpose.symbol")

## Original function to interface
splom(~ iris[1:4], data = iris, groups = Species,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~ iris[1:3] | Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
        ltext(x = seq(.6, .8, length.out = 4), 
              y = seq(.9, .6, length.out = 4), 
              labels = c("Three", "Varieties", "of", "Iris"),
              cex = 2)
      })

## The interface reverses the order of data and formula
ntbt_splom(data = iris, ~ iris[1:4], groups = Species,
           panel = panel.superpose,
           key = list(title = "Three Varieties of Iris",
                      columns = 3, 
                      points = list(pch = super.sym$pch[1:3],
                                    col = super.sym$col[1:3]),
                      text = list(c("Setosa", "Versicolor", "Virginica"))))
ntbt_splom(data = iris, ~ iris[1:3] | Species,
           layout=c(2,2), pscales = 0,
           varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
           page = function(...) {
             ltext(x = seq(.6, .8, length.out = 4), 
                   y = seq(.9, .6, length.out = 4), 
                   labels = c("Three", "Varieties", "of", "Iris"),
                   cex = 2)
           })

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_splom(~ iris[1:4], groups = Species,
             panel = panel.superpose,
             key = list(title = "Three Varieties of Iris",
                        columns = 3, 
                        points = list(pch = super.sym$pch[1:3],
                                      col = super.sym$col[1:3]),
                        text = list(c("Setosa", "Versicolor", "Virginica"))))
iris \%>\%
  ntbt_splom(~ iris[1:3] | Species,
             layout=c(2,2), pscales = 0,
             varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
             page = function(...) {
               ltext(x = seq(.6, .8, length.out = 4), 
                     y = seq(.9, .6, length.out = 4), 
                     labels = c("Three", "Varieties", "of", "Iris"),
                     cex = 2)
             })
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_splom}
\keyword{splom}
