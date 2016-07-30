\name{ntbt_stripchart}
\alias{ntbt_stripchart}
\title{Interface to stripchart for data science pipelines.}
\description{
  \code{ntbt_stripchart} is an interface to \code{graphics::\link{stripchart}}
  (1-D Scatter Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_stripchart(data, x, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{x}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}
  \item{...}{Other arguments passed to \code{\link{stripchart}}.}
}
\details{
  ntbt_stripchart reverses the order of \code{data} and \code{x}, so it can be
  used in a pipeline. It then calls \code{stripchart}.
}
\value{
  Object returned by \code{\link{stripchart}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{stripchart}}
}
\examples{
## Original function to interface
stripchart(decrease ~ treatment, data = OrchardSprays,
           main = "stripchart(OrchardSprays)",
           vertical = TRUE, log = "y")

## The interface reverses the order of data and formula
ntbt_stripchart(data = OrchardSprays, decrease ~ treatment,
                main = "stripchart(OrchardSprays)",
                vertical = TRUE, log = "y")

## so it can be used easily in a pipeline.
library(magrittr)
OrchardSprays \%>\%
  ntbt_stripchart(decrease ~ treatment,
                  main = "stripchart(OrchardSprays)",
                  vertical = TRUE, log = "y")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_stripchart}
\keyword{stripchart}
