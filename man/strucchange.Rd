\name{strucchange}
\alias{ntbt_breakpoints}
\alias{ntbt_efp}
\alias{ntbt_Fstats}
\alias{ntbt_mefp}
\alias{ntbt_recresid}
\alias{ntbt_sctest}
\title{Interfaces for strucchange package for data science pipelines.}
\description{
  Interfaces to \code{strucchange} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_breakpoints(data, ...)
ntbt_efp(data, ...)
ntbt_Fstats(data, ...)
ntbt_mefp(data, ...)
ntbt_recresid(data, ...)
ntbt_sctest(data, ...)
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
library(strucchange)

## ntbt_breakpoints: Dating Breaks
data("Nile")
d <- list(Nl = Nile)

## Original function to interface
breakpoints(Nl ~ 1, data = d)

## The interface puts data as first parameter
ntbt_breakpoints(d, Nl ~ 1)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_breakpoints(Nl ~ 1)


## ntbt_efp: Empirical Fluctuation Processes
## Original function to interface
ocus.nile <- efp(Nl ~ 1, d, type = "OLS-CUSUM")
plot(ocus.nile)

## The interface puts data as first parameter
ocus.nile <- ntbt_efp(d, Nl ~ 1, type = "OLS-CUSUM")
plot(ocus.nile)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_efp(Nl ~ 1, type = "OLS-CUSUM") \%>\%
  plot()


## ntbt_Fstats: F Statistics
## Original function to interface
fs.nile <- Fstats(Nl ~ 1, data = d)
plot(fs.nile)

## The interface puts data as first parameter
fs.nile <- ntbt_Fstats(d, Nl ~ 1)
plot(fs.nile)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_Fstats(Nl ~ 1) \%>\%
  plot()


## ntbt_mefp: Monitoring of Empirical Fluctuation Processes
df1 <- data.frame(y = rnorm(300))
df1[150:300, "y"] <- df1[150:300, "y"] + 1

## Original function to interface
mefp(y ~ 1, data = df1[1:50,, drop = FALSE], type = "ME", h = 1, alpha = 0.05)

## The interface puts data as first parameter
ntbt_mefp(df1[1:50,, drop = FALSE], y ~ 1, type = "ME", h = 1, alpha = 0.05)

## so it can be used easily in a pipeline.
df1[1:50,, drop = FALSE] \%>\%
  ntbt_mefp(y ~ 1, type = "ME", h = 1, alpha = 0.05)


## ntbt_recresid: Recursive Residuals
d1 <- list(x = rnorm(100) + rep(c(0, 2), each = 50))

## Original function to interface
recresid(x ~ 1, d1)

## The interface puts data as first parameter
ntbt_recresid(d1, x ~ 1)

## so it can be used easily in a pipeline.
d1 \%>\%
  ntbt_recresid(x ~ 1)


## ntbt_sctest: Structural Change Tests in Linear Regression Models
data("longley")
## Original function to interface
sctest(Employed ~ Year + GNP.deflator + GNP + Armed.Forces, data = longley,
       type = "Chow", point = 7)

## The interface puts data as first parameter
ntbt_sctest(longley, Employed ~ Year + GNP.deflator + GNP + Armed.Forces,
            type = "Chow", point = 7)

## so it can be used easily in a pipeline.
longley \%>\%
  ntbt_sctest(Employed ~ Year + GNP.deflator + GNP + Armed.Forces,
              type = "Chow", point = 7)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{strucchange}
\keyword{ntbt_breakpoints}
\keyword{ntbt_efp}
\keyword{ntbt_Fstats}
\keyword{ntbt_mefp}
\keyword{ntbt_recresid}
\keyword{ntbt_sctest}
