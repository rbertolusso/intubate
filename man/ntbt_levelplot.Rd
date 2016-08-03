\name{ntbt_levelplot}
\alias{ntbt_levelplot}
\title{Interface to levelplot for data science pipelines.}
\description{
  \code{ntbt_levelplot} is an interface to \code{lattice::\link{levelplot}}
  (Level Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_levelplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{levelplot}}.}
}
\details{
  ntbt_levelplot calls \code{levelplot}.
}
\value{
  Object returned by \code{\link{levelplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{levelplot}}
}
\examples{
library(lattice)

x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x = x, y = y)
grid$z <- cos(r^2) * exp(-r/(pi^3))

## Original function to interface
levelplot(z ~ x*y, grid, cuts = 50, scales = list(log = "e"), xlab = "",
          ylab = "", main = "Weird Function", sub = "with log scales",
          colorkey = FALSE, region = TRUE)

## The interface reverses the order of data and formula
ntbt_levelplot(grid, z ~ x*y, cuts = 50, scales = list(log = "e"), xlab = "",
               ylab = "", main = "Weird Function", sub = "with log scales",
               colorkey = FALSE, region = TRUE)

## so it can be used easily in a pipeline.
library(magrittr)
grid \%>\%
  ntbt_levelplot(z ~ x*y, cuts = 50, scales = list(log = "e"), xlab = "",
                 ylab = "", main = "Weird Function", sub = "with log scales",
                 colorkey = FALSE, region = TRUE)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_levelplot}
\keyword{levelplot}
