\name{vcd}
\alias{ntbt_agreementplot}
\alias{ntbt_assoc}
\alias{ntbt_cd_plot}
\alias{ntbt_cotabplot}
\alias{ntbt_loddsratio}
\alias{ntbt_mosaic}
\alias{ntbt_sieve}
\alias{ntbt_spine}
\alias{ntbt_structable}
\title{Interfaces for vcd package for data science pipelines.}
\description{
  Interfaces to \code{vcd} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_agreementplot(data, ...)
ntbt_assoc(data, ...)
ntbt_cd_plot(data, ...)
ntbt_cotabplot(data, ...)
ntbt_loddsratio(data, ...)
ntbt_mosaic(data, ...)
ntbt_sieve(data, ...)
ntbt_spine(data, ...)
ntbt_structable(data, ...)
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
library(vcd)


## ntbt_agreementplot: Bangdiwala's Observer Agreement Chart
## Original function to interface
agreementplot(Freq ~ Gender + Admit, as.data.frame(UCBAdmissions))

## The interface puts data as first parameter
ntbt_agreementplot(as.data.frame(UCBAdmissions), Freq ~ Gender + Admit)

## so it can be used easily in a pipeline.
as.data.frame(UCBAdmissions) \%>\%
  ntbt_agreementplot(Freq ~ Gender + Admit)


## ntbt_assoc: Extended Association Plots
## Original function to interface
assoc(Freq ~ Gender + Admit, data = as.data.frame(UCBAdmissions))

## The interface puts data as first parameter
ntbt_assoc(as.data.frame(UCBAdmissions), Freq ~ Gender + Admit)

## so it can be used easily in a pipeline.
as.data.frame(UCBAdmissions) \%>\%
  ntbt_assoc(Freq ~ Gender + Admit)


## ntbt_cd_plot: Conditional Density Plots
data("Arthritis")
## Original function to interface
cd_plot(Improved ~ Age, data = Arthritis)

## The interface puts data as first parameter
ntbt_cd_plot(Arthritis, Improved ~ Age)

## so it can be used easily in a pipeline.
Arthritis \%>\%
  ntbt_cd_plot(Improved ~ Age)


## ntbt_cotabplot: Coplot for Contingency Tables
## Original function to interface
cotabplot(~ Admit + Gender | Dept, data = UCBAdmissions)

## The interface puts data as first parameter
ntbt_cotabplot(UCBAdmissions, ~ Admit + Gender | Dept)

## so it can be used easily in a pipeline.
UCBAdmissions \%>\%
  ntbt_cotabplot(~ Admit + Gender | Dept)


## ntbt_loddsratio: Calculate Generalized Log Odds Ratios for Frequency Tables
data(Punishment, package = "vcd")

## Original function to interface
loddsratio(Freq ~ memory + attitude | age + education, data = Punishment)

## The interface puts data as first parameter
ntbt_loddsratio(Punishment, Freq ~ memory + attitude | age + education)

## so it can be used easily in a pipeline.
Punishment \%>\%
  ntbt_loddsratio(Freq ~ memory + attitude | age + education)


## ntbt_mosaic: Extended Mosaic Plots
library(MASS)
data("Titanic")
mosaic(Titanic)

## Original function to interface
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)

## The interface puts data as first parameter
ntbt_mosaic(Titanic, ~ Sex + Age + Survived,
            main = "Survival on the Titanic", shade = TRUE, legend = TRUE)

## so it can be used easily in a pipeline.
Titanic \%>\%
  ntbt_mosaic(~ Sex + Age + Survived,
              main = "Survival on the Titanic", shade = TRUE, legend = TRUE)


## ntbt_sieve: Extended Sieve Plots
data("VisualAcuity")

## Original function to interface
sieve(Freq ~ right + left,  data = VisualAcuity)

## The interface puts data as first parameter
ntbt_sieve(VisualAcuity, Freq ~ right + left)

## so it can be used easily in a pipeline.
VisualAcuity \%>\%
  ntbt_sieve(Freq ~ right + left)


## ntbt_spine: Spine Plots and Spinograms
data("Arthritis")

## Original function to interface
spine(Improved ~ Treatment, data = Arthritis)

## The interface puts data as first parameter
ntbt_spine(Arthritis, Improved ~ Treatment)

## so it can be used easily in a pipeline.
Arthritis \%>\%
  ntbt_spine(Improved ~ Treatment)


## ntbt_structable: Structured Contingency Tables
## Original function to interface
structable(Sex + Class ~ Survived + Age, data = Titanic)

## The interface puts data as first parameter
ntbt_structable(Titanic, Sex + Class ~ Survived + Age)

## so it can be used easily in a pipeline.
Titanic \%>\%
  ntbt_structable(Sex + Class ~ Survived + Age)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{vcd}
\keyword{agreementplot}
\keyword{assoc}
\keyword{cd_plot}
\keyword{cotabplot}
\keyword{loddsratio}
\keyword{mosaic}
\keyword{sieve}
\keyword{spine}
\keyword{structable}
