\name{ntbt_mosaicplot}
\alias{ntbt_mosaicplot}
\title{Interface to mosaicplot for data science pipelines.}
\description{
  \code{ntbt_mosaicplot} is an interface to \code{graphics::\link{mosaicplot}}
  (Mosaic Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_mosaicplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{mosaicplot}}.}
}
\details{
  ntbt_mosaicplot calls \code{mosaicplot}.
}
\value{
  Object returned by \code{\link{mosaicplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{mosaicplot}}
}
\examples{
## Original function to interface
mosaicplot(~ Sex + Age + Survived, data = Titanic, color = TRUE)

## The interface reverses the order of data and formula
ntbt_mosaicplot(data = Titanic, ~ Sex + Age + Survived, color = TRUE)

## so it can be used easily in a pipeline.
library(magrittr)
Titanic \%>\%
  ntbt_mosaicplot(~ Sex + Age + Survived, color = TRUE)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_mosaicplot}
\keyword{mosaicplot}
