\name{ntbt_wireframe}
\alias{ntbt_wireframe}
\title{Interface to wireframe for data science pipelines.}
\description{
  \code{ntbt_wireframe} is an interface to \code{lattice::\link{wireframe}}
  (Wireframe Surface Plot) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_wireframe(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{wireframe}}.}
}
\details{
  ntbt_wireframe reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{wireframe}.
}
\value{
  Object returned by \code{\link{wireframe}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{wireframe}}
}
\examples{
library(lattice)
g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$gr + g$y^2) * g$gr)

## Original function to interface
wireframe(z ~ x * y, data = g, groups = gr,
          scales = list(arrows = FALSE),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -60))

## The interface reverses the order of data and formula
ntbt_wireframe(data = g, z ~ x * y, groups = gr,
               scales = list(arrows = FALSE),
               drape = TRUE, colorkey = TRUE,
               screen = list(z = 30, x = -60))

## so it can be used easily in a pipeline.
library(magrittr)
g \%>\%
  ntbt_wireframe(z ~ x * y, groups = gr,
                 scales = list(arrows = FALSE),
                 drape = TRUE, colorkey = TRUE,
                 screen = list(z = 30, x = -60))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_wireframe}
\keyword{wireframe}
