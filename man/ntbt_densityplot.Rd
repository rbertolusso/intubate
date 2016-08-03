\name{ntbt_densityplot}
\alias{ntbt_densityplot}
\title{Interface to densityplot for data science pipelines.}
\description{
  \code{ntbt_densityplot} is an interface to \code{lattice::\link{densityplot}}
  (Kernel Density Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_densityplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{densityplot}}.}
}
\details{
  ntbt_densityplot calls \code{densityplot}.
}
\value{
  Object returned by \code{\link{densityplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{densityplot}}
}
\examples{
library(lattice)

## Original function to interface
densityplot(~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)", bw = 5)

## The interface reverses the order of data and formula
ntbt_densityplot(data = singer, ~ height | voice.part, layout = c(2, 4),  
                 xlab = "Height (inches)", bw = 5)

## so it can be used easily in a pipeline.
library(magrittr)
singer \%>\%
  ntbt_densityplot(~ height | voice.part, layout = c(2, 4),  
                   xlab = "Height (inches)", bw = 5)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_densityplot}
\keyword{densityplot}
