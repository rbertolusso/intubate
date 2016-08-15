\name{caper}
\alias{ntbt_brunch}
\alias{ntbt_crunch}
\alias{ntbt_macrocaic}
\alias{ntbt_pgls}
\title{Interfaces for caper package for data science pipelines.}
\description{
  Interfaces to \code{caper} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_brunch(data, ...)
ntbt_crunch(data, ...)
ntbt_macrocaic(data, ...)
ntbt_pgls(data, ...)
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
library(caper)

## ntbt_brunch: Calculate a linear model using the brunch algorithm
data(perissodactyla)
perisso <- comparative.data(perissodactyla.tree, perissodactyla.data, Binomial)

## Original function to interface
brunch(log.female.wt ~ Territoriality, data = perisso)

## The interface puts data as first parameter
ntbt_brunch(perisso, log.female.wt ~ Territoriality)

## so it can be used easily in a pipeline.
perisso \%>\%
  ntbt_brunch(log.female.wt ~ Territoriality)


## ntbt_crunch: Calculate a linear model using the crunch algorithm
data(shorebird)
shorebird <- comparative.data(shorebird.tree, shorebird.data, Species)

## Original function to interface
crunch(Egg.Mass ~ F.Mass + M.Mass, data = shorebird)

## The interface puts data as first parameter
ntbt_crunch(shorebird, Egg.Mass ~ F.Mass + M.Mass)

## so it can be used easily in a pipeline.
shorebird \%>\%
  ntbt_crunch(Egg.Mass ~ F.Mass + M.Mass)


## ntbt_macrocaic: Comparative analysis using independent
##                 contrasts on species richness data
data(IsaacEtAl)
primates <- comparative.data(primates.tree, primates.data, binomial, na.omit=FALSE)

## Original function to interface
macrocaic(species.rich ~ body.mass, data = primates)

## The interface puts data as first parameter
ntbt_macrocaic(primates, species.rich ~ body.mass)

## so it can be used easily in a pipeline.
primates \%>\%
  ntbt_macrocaic(species.rich ~ body.mass)



## ntbt_pgls: Phylogenetic generalized linear models
data(shorebird)
shorebird <- comparative.data(shorebird.tree, shorebird.data, Species, vcv=TRUE, vcv.dim=3)

## Original function to interface
pgls(log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), shorebird, lambda='ML')

## The interface puts data as first parameter
ntbt_pgls(shorebird, log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), lambda='ML')

## so it can be used easily in a pipeline.
shorebird \%>\%
  ntbt_pgls(log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), lambda='ML')
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{caper}
\keyword{brunch}
\keyword{crunch}
\keyword{macrocaic}
\keyword{pgls}
