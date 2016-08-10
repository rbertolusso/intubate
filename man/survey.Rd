\name{survey}
\alias{ntbt_svydesign}
\alias{ntbt_twophase}
\title{Interfaces for survey package for data science pipelines.}
\description{
  Interfaces to \code{survey} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_svydesign(data, ...)
ntbt_twophase(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list, ...}
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
library(magrittr)
library(survey)

## svydesign
data(api)
## Original function to interface
# stratified sample
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
# one-stage cluster sample
dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
# two-stage cluster sample: weights computed from population sizes.
dclus2 <- svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

## The interface puts data as first parameter
# stratified sample
dstrat <- ntbt_svydesign(data=apistrat, id=~1,strata=~stype, weights=~pw, fpc=~fpc)
# one-stage cluster sample
dclus1 <- ntbt_svydesign(data=apiclus1, id=~dnum, weights=~pw, fpc=~fpc)
# two-stage cluster sample: weights computed from population sizes.
dclus2 <- ntbt_svydesign(data=apiclus2, id=~dnum+snum, fpc=~fpc1+fpc2)

## so it can be used easily in a pipeline.
dstrat <- apistrat %>%
  ntbt_svydesign(id=~1,strata=~stype, weights=~pw, fpc=~fpc)
# one-stage cluster sample
dclus1 <- apiclus1 %>%
  ntbt_svydesign(id=~dnum, weights=~pw, fpc=~fpc)
# two-stage cluster sample: weights computed from population sizes.
dclus2 <- apiclus2 %>%
  ntbt_svydesign(id=~dnum+snum, fpc=~fpc1+fpc2)

## twofase
## two-phase simple random sampling.
data(pbc, package="survival")
pbc$randomized <- with(pbc, !is.na(trt) & trt>0)
pbc$id<-1:nrow(pbc)

## Original function to interface
d2pbc <- twophase(id=list(~id,~id), data=pbc, subset=~randomized)
svymean(~bili, d2pbc)

## The interface puts data as first parameter
d2pbc <- ntbt_twophase(data=pbc, id=list(~id,~id), subset=~randomized)
svymean(~bili, d2pbc)

## so it can be used easily in a pipeline.
d2pbc <- pbc %>%
  ntbt_twophase(id=list(~id,~id), subset=~randomized)
svymean(~bili, d2pbc)

## Note: you can also to the following
pbc %>%
  ntbt_twophase(id=list(~id,~id), subset=~randomized) %>%
  ntbt(svymean, ~bili)    ## Call the function with ntbt()
## or
ntbt_svymean <- intubate  ## Define interface "on demand"
pbc %>%
  ntbt_twophase(id=list(~id,~id), subset=~randomized) %>%
  ntbt_svymean(~bili)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{survey}
\keyword{svydesign}
\keyword{twophase}
