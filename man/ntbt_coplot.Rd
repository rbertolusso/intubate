\name{ntbt_coplot}
\alias{ntbt_coplot}
\title{Interface to coplot for data science pipelines.}
\description{
  \code{ntbt_coplot} is an interface to \code{graphics::\link{coplot}}
  (Conditioning coplots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_coplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{coplot}}.}
}
\details{
  ntbt_coplot calls \code{coplot}.
}
\value{
  Object returned by \code{\link{coplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{coplot}}
}
\examples{
Index <- seq(length = nrow(warpbreaks))

## Original function to interface
coplot(breaks ~ Index | wool * tension, data = warpbreaks,
       show.given = 0:1)
coplot(breaks ~ Index | wool * tension, data = warpbreaks,
       col = "red", bg = "pink", pch = 21,
       bar.bg = c(fac = "light blue"))

## The interface reverses the order of data and formula
ntbt_coplot(data = warpbreaks, breaks ~ Index | wool * tension,
            show.given = 0:1)
ntbt_coplot(data = warpbreaks, breaks ~ Index | wool * tension,
            col = "red", bg = "pink", pch = 21,
            bar.bg = c(fac = "light blue"))

## so it can be used easily in a pipeline.
library(magrittr)
warpbreaks \%T>\%  ## Note the tee operator.
  ntbt_coplot(breaks ~ Index | wool * tension,
              show.given = 0:1) \%>\%
  ntbt_coplot(breaks ~ Index | wool * tension,
              col = "red", bg = "pink", pch = 21,
              bar.bg = c(fac = "light blue"))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_coplot}
\keyword{coplot}
