\name{car}
\alias{ntbt_Boxplot}
\alias{ntbt_boxTidwell}
\alias{ntbt_densityPlot}
\alias{ntbt_invTranPlot}
\alias{ntbt_leveneTest}
\alias{ntbt_powerTransform}
\alias{ntbt_scatter3d}
\alias{ntbt_scatterplot}
\alias{ntbt_scatterplotMatrix}
\alias{ntbt_spreadLevelPlot}
\alias{ntbt_symbox}
\title{Interfaces for car package for data science pipelines.}
\description{
  Interfaces to \code{car} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_Boxplot(data, ...)
ntbt_boxTidwell(data, ...)
ntbt_densityPlot(data, ...)
ntbt_invTranPlot(data, ...)
ntbt_leveneTest(data, ...)
ntbt_powerTransform(data, ...)
ntbt_scatter3d(data, ...)
ntbt_scatterplot(data, ...)
ntbt_scatterplotMatrix(data, ...)
ntbt_spreadLevelPlot(data, ...)
ntbt_symbox(data, ...)
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
library(car)

## ntbt_Boxplot: Boxplots With Point Identification
## Original function to interface
Boxplot(income ~ type, data = Prestige)

## The interface puts data as first parameter
ntbt_Boxplot(Prestige, income ~ type)

## so it can be used easily in a pipeline.
Prestige \%>\%
  ntbt_Boxplot(income ~ type)


## ntbt_boxTidwell: Box-Tidwell Transformations
## Original function to interface
boxTidwell(prestige ~ income + education, ~ type + poly(women, 2), data = Prestige)

## The interface puts data as first parameter
ntbt_boxTidwell(Prestige, prestige ~ income + education, ~ type + poly(women, 2))

## so it can be used easily in a pipeline.
Prestige \%>\%
  ntbt_boxTidwell(prestige ~ income + education, ~ type + poly(women, 2))


## ntbt_densityPlot: Nonparametric Density Estimates
## Original function to interface
densityPlot(income ~ type, data = Prestige)

## The interface puts data as first parameter
ntbt_densityPlot(Prestige, income ~ type)

## so it can be used easily in a pipeline.
Prestige \%>\%
  ntbt_densityPlot(income ~ type)


## ntbt_invTranPlot: Choose a Predictor Transformation Visually or Numerically
## Original function to interface
invTranPlot(infant.mortality ~ gdp, data = UN)

## The interface puts data as first parameter
ntbt_invTranPlot(UN, infant.mortality ~ gdp)

## so it can be used easily in a pipeline.
UN \%>\%
  ntbt_invTranPlot(infant.mortality ~ gdp)

## ntbt_leveneTest: Levene's test for homogeneity of variance across groups
## Original function to interface
leveneTest(conformity ~ fcategory*partner.status, data = Moore)

## The interface puts data as first parameter
ntbt_leveneTest(Moore, conformity ~ fcategory*partner.status)

## so it can be used easily in a pipeline.
Moore \%>\%
  ntbt_leveneTest(conformity ~ fcategory*partner.status)

## ntbt_powerTransform: Finding Univariate or Multivariate Power Transformations
## Original function to interface
powerTransform(cycles ~ len + amp + load, Wool)

## The interface puts data as first parameter
ntbt_powerTransform(Wool, cycles ~ len + amp + load)

## so it can be used easily in a pipeline.
Wool \%>\%
  ntbt_powerTransform(cycles ~ len + amp + load)


## ntbt_scatter3d: Three-Dimensional Scatterplots and Point Identification
## Original function to interface
## NOTE: need rgl, mgcv, and interactive mode. Commented out.
#scatter3d(prestige ~ income + education, data = Duncan)

## The interface puts data as first parameter
#ntbt_scatter3d(Duncan, prestige ~ income + education)

## so it can be used easily in a pipeline.
#Duncan \%>\%
#  ntbt_scatter3d(prestige ~ income + education)


## ntbt_scatterplot: Scatterplots with Boxplots
## Original function to interface
scatterplot(prestige ~ income, data = Prestige, ellipse = TRUE)

## The interface puts data as first parameter
ntbt_scatterplot(Prestige, prestige ~ income, ellipse = TRUE)

## so it can be used easily in a pipeline.
Prestige \%>\%
  ntbt_scatterplot(prestige ~ income, ellipse = TRUE)

## ntbt_scatterplotMatrix: Scatterplot Matrices
## Original function to interface
scatterplotMatrix(~ income + education + prestige | type, data = Duncan)

## The interface puts data as first parameter
ntbt_scatterplotMatrix(Duncan, ~ income + education + prestige | type)

## so it can be used easily in a pipeline.
Duncan \%>\%
  ntbt_scatterplotMatrix(~ income + education + prestige | type)

## ntbt_spreadLevelPlot: Spread-Level Plots
## Original function to interface
spreadLevelPlot(interlocks + 1 ~ nation, data = Ornstein)

## The interface puts data as first parameter
ntbt_spreadLevelPlot(Ornstein, interlocks + 1 ~ nation)

## so it can be used easily in a pipeline.
Ornstein \%>\%
  ntbt_spreadLevelPlot(interlocks + 1 ~ nation)

## ntbt_symbox: Boxplots for transformations to symmetry
## Original function to interface
symbox(~ income, data = Prestige)

## The interface puts data as first parameter
ntbt_symbox(Prestige, ~ income)

## so it can be used easily in a pipeline.
Prestige \%>\%
  ntbt_symbox(~ income)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{car}
\keyword{Boxplot}
\keyword{boxTidwell}
\keyword{densityPlot}
\keyword{invTranPlot}
\keyword{leveneTest}
\keyword{powerTransform}
\keyword{scatter3d}
\keyword{scatterplot}
\keyword{scatterplotMatrix}
\keyword{spreadLevelPlot}
\keyword{symbox}
