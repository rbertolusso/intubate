\name{ntbt_wilcox.test}
\alias{ntbt_wilcox.test}
\title{Interface to wilcox.test for data science pipelines.}
\description{
  \code{ntbt_wilcox.test} is an interface to \code{stats::\link{wilcox.test}}
  (Wilcoxon Rank Sum and Signed Rank Tests) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_wilcox.test(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{wilcox.test}}.}
}
\details{
  ntbt_wilcox.test calls \code{wilcox.test}.
}
\value{
  Object returned by \code{\link{wilcox.test}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{wilcox.test}}
}
\examples{
## Original function to interface
wilcox.test(extra ~ group, data = sleep)

## The interface reverses the order of data and formula
ntbt_wilcox.test(data = sleep, extra ~ group)

## so it can be used easily in a pipeline.
library(magrittr)
sleep \%>\%
  ntbt_wilcox.test(extra ~ group)
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_wilcox.test}
\keyword{wilcox.test}
