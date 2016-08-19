\name{psychotree}
\alias{ntbt_bttree}
\alias{ntbt_mpttree}
\alias{ntbt_pctree}
\alias{ntbt_raschtree}
\alias{ntbt_rstree}
\title{Interfaces for psychotree package for data science pipelines.}
\description{
  Interfaces to \code{psychotree} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bttree(data, ...)
ntbt_mpttree(data, ...)
ntbt_pctree(data, ...)
ntbt_raschtree(data, ...)
ntbt_rstree(data, ...)
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
library(psychotree)


## ntbt_bttree: Bradley-Terry Tree Models
data("Topmodel2007", package = "psychotree")

## Original function to interface
tm_tree <- bttree(preference ~ ., data = Topmodel2007,
                  minsize = 5, ref = "Barbara")
plot(tm_tree, abbreviate = 1, yscale = c(0, 0.5))

## The interface puts data as first parameter
tm_tree <- ntbt_bttree(Topmodel2007, preference ~ .,
                       minsize = 5, ref = "Barbara")
plot(tm_tree, abbreviate = 1, yscale = c(0, 0.5))

## so it can be used easily in a pipeline.
Topmodel2007 \%>\%
  ntbt_bttree(preference ~ ., minsize = 5, ref = "Barbara") \%>\%
  plot(abbreviate = 1, yscale = c(0, 0.5))



## ntbt_mpttree: MPT Tree Models
data("SourceMonitoring", package="psychotools")

## Original function to interface
sm_tree <- mpttree(y ~ sources + gender + age, data = SourceMonitoring,
                   spec = mptspec("SourceMon", .restr = list(d1 = d, d2 = d)))
plot(sm_tree, index = c("D1", "D2", "d", "b", "g"))

## The interface puts data as first parameter
sm_tree <- ntbt_mpttree(SourceMonitoring, y ~ sources + gender + age,
                        spec = mptspec("SourceMon", .restr = list(d1 = d, d2 = d)))
plot(sm_tree, index = c("D1", "D2", "d", "b", "g"))

## so it can be used easily in a pipeline.
SourceMonitoring \%>\%
  ntbt_mpttree(y ~ sources + gender + age,
               spec = mptspec("SourceMon", .restr = list(d1 = d, d2 = d))) \%>\%
  plot(index = c("D1", "D2", "d", "b", "g"))



## ntbt_pctree: Partial Credit Tree Models
data("VerbalAggression", package = "psychotools")
VerbalAggression$s2 <- VerbalAggression$resp[, 7:12]
VerbalAggression <- subset(VerbalAggression, rowSums(s2) > 0 & rowSums(s2) < 12)

## Original function to interface
pct <- pctree(s2 ~ anger + gender, data = VerbalAggression)
plot(pct, type = "profile")

## The interface puts data as first parameter
pct <- ntbt_pctree(VerbalAggression, s2 ~ anger + gender)
plot(pct, type = "profile")

## so it can be used easily in a pipeline.
VerbalAggression \%>\%
  ntbt_pctree(s2 ~ anger + gender) \%>\%
  plot(type = "profile")



## ntbt_raschtree: Rasch Tree Models
data("DIFSim", package = "psychotree")

## Original function to interface
rt <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
plot(rt)

## The interface puts data as first parameter
rt <- ntbt_raschtree(DIFSim, resp ~ age + gender + motivation)
plot(rt)

## so it can be used easily in a pipeline.
DIFSim \%>\%
  ntbt_raschtree(resp ~ age + gender + motivation) \%>\%
  plot()



## ntbt_rstree: Rating Scale Tree Models
data("VerbalAggression", package = "psychotools")
VerbalAggression$s1 <- VerbalAggression$resp[, 1:6]
VerbalAggression <- subset(VerbalAggression, rowSums(s1) > 0 & rowSums(s1) < 12)

## Original function to interface
rst <- rstree(s1 ~ anger + gender, data = VerbalAggression)
plot(rst, type = "profile")

## The interface puts data as first parameter
rst <- ntbt_rstree(VerbalAggression, s1 ~ anger + gender)
plot(rst, type = "profile")

## so it can be used easily in a pipeline.
VerbalAggression \%>\%
  ntbt_rstree(s1 ~ anger + gender) \%>\%
  plot(type = "profile")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{psychotree}
\keyword{bttree}
\keyword{mpttree}
\keyword{pctree}
\keyword{raschtree}
\keyword{rstree}
