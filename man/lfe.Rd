\name{lfe}
\alias{ntbt_felm}
\title{Interfaces for lfe package for data science pipelines.}
\description{
  Interfaces to \code{lfe} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_felm(data, ...)
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
\seealso{
  \code{lfe}
}
\examples{
\dontrun{
library(magrittr)
library(lfe)

oldopts <- options(lfe.threads=1)

set.seed(123)
## create covariates
x <- rnorm(1000)
x2 <- rnorm(length(x))
## individual and firm
id <- factor(sample(20,length(x),replace=TRUE))
firm <- factor(sample(13,length(x),replace=TRUE))
## effects for them
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))
## left hand side
u <- rnorm(length(x))
y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u
data <- data.frame(x = x, x2 = x2, id = id,
                   firm = firm, u = u, y = y)

## Original function to interface
est <- felm(y ~ x + x2 | id + firm, data)
summary(est)

## The interface reverses the order of data and formula
est <- ntbt_felm(data, y ~ x + x2 | id + firm)
summary(est)

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_felm(y ~ x + x2 | id + firm) \%>\%
  summary()
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{lfe}
\keyword{felm}
