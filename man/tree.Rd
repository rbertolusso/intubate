\name{tree}
\alias{ntbt_tree}
\title{Interfaces for tree package for data science pipelines.}
\description{
  Interfaces to \code{tree} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_tree(data, ...)
}
\arguments{
  \item{data}{data frame, tibble, list, ...}
  \item{...}{Other arguments passed to the corresponding interfaced function.}
}
\details{
  Interfaces call their corresponding interfaced function.
}
\value{
  Object returned by interfaced function.
}
\author{
Roberto Bertolusso
}
\examples{
\dontrun{
library(intubate)
library(magrittr)
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
}
\keyword{intubate}
\keyword{magrittr}
\keyword{tree}
