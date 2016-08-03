\name{ntbt_sunflowerplot}
\alias{ntbt_sunflowerplot}
\title{Interface to sunflowerplot for data science pipelines.}
\description{
  \code{ntbt_sunflowerplot} is an interface to \code{graphics::\link{sunflowerplot}}
  (Produce a Sunflower Scatter Plot) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_sunflowerplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{sunflowerplot}}.}
}
\details{
  ntbt_sunflowerplot calls \code{sunflowerplot}.
}
\value{
  Object returned by \code{\link{sunflowerplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{sunflowerplot}}
}
\examples{
## Original function to interface
sunflowerplot(Petal.Width ~ Petal.Length, data = iris,
              cex = .2, cex.fact = 1, size = .035,
              seg.lwd = .8)

## The interface reverses the order of data and formula
ntbt_sunflowerplot(data = iris, Petal.Width ~ Petal.Length,
                   cex = .2, cex.fact = 1, size = .035,
                   seg.lwd = .8)

## so it can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_sunflowerplot(Petal.Width ~ Petal.Length,
                     cex = .2, cex.fact = 1, size = .035,
                     seg.lwd = .8)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_sunflowerplot}
\keyword{sunflowerplot}
