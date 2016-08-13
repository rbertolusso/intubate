\name{survey}
\alias{ntbt_svyby}
\alias{ntbt_svycoxph}
\alias{ntbt_svydesign}
\alias{ntbt_svyglm}
\alias{ntbt_svymean}
\alias{ntbt_svyquantile}
\alias{ntbt_svyratio}
\alias{ntbt_svytotal}
\alias{ntbt_twophase}
\title{Interfaces for survey package for data science pipelines.}
\description{
  Interfaces to \code{survey} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_svyby(data, ...)         ## data <-> design
ntbt_svycoxph(data, ...)      ## data <-> design
ntbt_svydesign(data, ...)
ntbt_svyglm(data, ...)        ## data <-> design
ntbt_svymean(data, ...)       ## data <-> design
ntbt_svyquantile(data, ...)   ## data <-> design
ntbt_svyratio(data, ...)      ## data <-> design
ntbt_svytotal(data, ...)      ## data <-> design
ntbt_twophase(data, ...)
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
dstrat <- apistrat \%>\%
  ntbt_svydesign(id=~1,strata=~stype, weights=~pw, fpc=~fpc)
# one-stage cluster sample
dclus1 <- apiclus1 \%>\%
  ntbt_svydesign(id=~dnum, weights=~pw, fpc=~fpc)
# two-stage cluster sample: weights computed from population sizes.
dclus2 <- apiclus2 \%>\%
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
d2pbc <- pbc \%>\%
  ntbt_twophase(id=list(~id,~id), subset=~randomized)
svymean(~bili, d2pbc)


## ntbt_svyby, ntbt_svyglm, ntbt_svymean,
## ntbt_svyquantile, ntbt_svyratio, ntbt_svytotal

## From vignette of survey
vars<-names(apiclus1)[c(12:13,16:23,27:37)] 

## original
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
summary(dclus1)
## direct call
dclus1 <- apiclus1 \%>\%
  ntbt(svydesign, id = ~dnum, weights = ~pw, fpc = ~fpc)
summary(dclus1)
## interface
dclus1 <- apiclus1 \%>\%
  ntbt_svydesign(id = ~dnum, weights = ~pw, fpc = ~fpc)
summary(dclus1)

## original
svymean(~api00, dclus1)
## direct call
dclus1 \%>\%
  ntbt(svymean, x=~api00)
## interface
dclus1 \%>\%
  ntbt_svymean(x=~api00)

## original
svyquantile(~api00, dclus1, quantile=c(0.25,0.5,0.75), ci=TRUE)
## direct call
dclus1 \%>\%
  ntbt(svyquantile, ~api00, quantile=c(0.25,0.5,0.75), ci=TRUE)
## interface
dclus1 \%>\%
  ntbt(svyquantile, ~api00, quantile=c(0.25,0.5,0.75), ci=TRUE)

## original
svytotal(~stype, dclus1)
svytotal(~enroll, dclus1)
## direct call
dclus1 \%>\%
  ntbt(svytotal, ~stype)
dclus1 \%>\%
  ntbt(svytotal,~enroll)
## interface
dclus1 \%>\%
  ntbt(svytotal, ~stype)
dclus1 \%>\%
  ntbt(svytotal,~enroll)

## original
svyratio(~api.stu, ~enroll, dclus1)
svyratio(~api.stu, ~enroll, design=subset(dclus1, stype=="H"))
## direct call
dclus1 \%>\%
  ntbt(svyratio, ~api.stu, ~enroll)
dclus1 \%>\%
  ntbt(svyratio, ~api.stu, ~enroll, design=subset("#", stype=="H"))
## interface
dclus1 \%>\%
  ntbt_svyratio(~api.stu, ~enroll)
dclus1 \%>\%
  ntbt_svyratio(~api.stu, ~enroll, design=subset("#", stype=="H"))

## original
svymean(make.formula(vars),dclus1,na.rm=TRUE)
## direct call
dclus1 \%>\%
  ntbt(svymean, make.formula(vars), na.rm=TRUE)
## interface
dclus1 \%>\%
  ntbt_svymean(make.formula(vars), na.rm=TRUE)

## original
svyby(~ell+meals, ~stype, design=dclus1, svymean)
## direct call
dclus1 \%>\%
  ntbt(svyby, ~ell+meals, ~stype, svymean)
## interface
dclus1 \%>\%
  ntbt_svyby(~ell+meals, ~stype, svymean)

## original
regmodel <- svyglm(api00~ell+meals, design=dclus1)
summary(regmodel)
logitmodel <- svyglm(I(sch.wide=="Yes")~ell+meals, design=dclus1,
                     family=quasibinomial()) 
summary(logitmodel)
## direct call
dclus1 \%>\%
  ntbt(svyglm, api00~ell+meals) \%>\%
  summary()
dclus1 \%>\%
  ntbt(svyglm, I(sch.wide=="Yes")~ell+meals, family=quasibinomial()) \%>\%
  summary()
## interface
dclus1 \%>\%
  ntbt_svyglm(api00~ell+meals) \%>\%
  summary()
dclus1 \%>\%
  ntbt_svyglm(I(sch.wide=="Yes")~ell+meals, family=quasibinomial()) \%>\%
  summary()

## ntbt_svycoxph
## stratified on case status
data(nwtco)
## original
dcchs <- twophase(id=list(~seqno,~seqno), strata=list(NULL,~rel),
                  subset=~I(in.subcohort | rel), data=nwtco)
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12), design=dcchs)
## direct call
nwtco \%>\%
  ntbt(twophase,id = list(~seqno,~seqno), strata = list(NULL,~rel),
       subset = ~I(in.subcohort | rel)) \%>\%
  ntbt(svycoxph, Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12))
## interface
nwtco \%>\%
  ntbt_twophase(id = list(~seqno,~seqno), strata = list(NULL,~rel),
       subset = ~I(in.subcohort | rel)) \%>\%
  ntbt_svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{survey}
\keyword{svyby}
\keyword{svycoxph}
\keyword{svydesign}
\keyword{svyglm}
\keyword{svymean}
\keyword{svyquantile}
\keyword{svyratio}
\keyword{svytotal}
\keyword{twophase}
