\name{ntbt_tree}
\alias{ntbt_tree}
\title{Interface to tree for data science pipelines.}
\description{
  \code{ntbt_tree} is an interface to \code{tree::\link{tree}}
  (Fit a Classification or Regression Tree) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_tree(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{tree}}.}
}
\details{
  ntbt_tree calls \code{tree}.
}
\value{
  Object returned by tree function.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{tree}}
}
\examples{
library(tree)
data(cpus, package="MASS")

## Original function to interface
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
cpus.ltr
summary(cpus.ltr)
plot(cpus.ltr)
text(cpus.ltr)

## The interface reverses the order of data and formula
cpus.ltr <- ntbt_tree(cpus, log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax)
cpus.ltr
summary(cpus.ltr)
plot(cpus.ltr);  text(cpus.ltr)

## so it can be used easily in a pipeline.
library(magrittr)
cpus \%>\%
  ntbt_tree(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax) \%>\%
  summary()

cpus \%>\%
  ntbt_tree(log10(perf) ~ syct + mmin + mmax + cach + chmin + chmax) \%T>\%
  plot() \%>\%
  text()

iris \%>\%
  ntbt_tree(Species ~.) \%>\%
  summary()
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_tree}
\keyword{tree}
