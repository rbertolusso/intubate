\name{vegan}
\alias{ntbt_adipart}
\alias{ntbt_adonis}
\alias{ntbt_adonis2}
\alias{ntbt_bioenv}
\alias{ntbt_capscale}
\alias{ntbt_cca}
\alias{ntbt_dbrda}
\alias{ntbt_envfit}
\alias{ntbt_gdispweight}
\alias{ntbt_multipart}
\alias{ntbt_ordicloud}
\alias{ntbt_ordisplom}
\alias{ntbt_ordisurf}
\alias{ntbt_ordixyplot}
\title{Interfaces for vegan package for data science pipelines.}
\description{
  Interfaces to \code{vegan} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_adipart(data, ...)
ntbt_adonis(data, ...)
ntbt_adonis2(data, ...)
ntbt_bioenv(data, ...)
ntbt_capscale(data, ...)
ntbt_cca(data, ...)
ntbt_dbrda(data, ...)
ntbt_envfit(data, ...)
ntbt_gdispweight(data, ...)
ntbt_multipart(data, ...)
ntbt_ordicloud(data, ...)
ntbt_ordisplom(data, ...)
ntbt_ordisurf(data, ...)
ntbt_ordixyplot(data, ...)
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
library(vegan)

## There is cheating going on on these examples,
## as the cases need two datasets, and only one
## is being piped... I may get back to this down the line.
## For now, please close an eye.

## ntbt_adipart: Additive Diversity Partitioning and Hierarchical Null Model Testing
data(mite)
data(mite.xy)
## Function to get equal area partitions of the mite data
cutter <- function (x, cut = seq(0, 10, by = 2.5)) {
    out <- rep(1, length(x))
    for (i in 2:(length(cut) - 1))
        out[which(x > cut[i] & x <= cut[(i + 1)])] <- i
    return(out)}
## The hierarchy of sample aggregation
levsm <- with(mite.xy, data.frame(
    l1=1:nrow(mite),
    l2=cutter(y, cut = seq(0, 10, by = 2.5)),
    l3=cutter(y, cut = seq(0, 10, by = 5)),
    l4=cutter(y, cut = seq(0, 10, by = 10))))

## Original function to interface
set.seed(1)
adipart(mite ~ ., levsm, index="richness", nsimul=19)

## The interface puts data as first parameter
set.seed(1)
ntbt_adipart(levsm, mite ~ ., index="richness", nsimul=19)

## so it can be used easily in a pipeline.
set.seed(1)
levsm \%>\%
  ntbt_adipart(mite ~ ., index="richness", nsimul=19)


## ntbt_adonis: Permutational Multivariate Analysis of Variance Using Distance Matrices
data(dune)
data(dune.env)

## Original function to interface
set.seed(1)
adonis(dune ~ Management*A1, data = dune.env)
adonis2(dune ~ Management*A1, data = dune.env)

## The interface puts data as first parameter
set.seed(1)
ntbt_adonis(dune.env, dune ~ Management*A1)
ntbt_adonis2(dune.env, dune ~ Management*A1)

## so it can be used easily in a pipeline.
set.seed(1)
dune.env \%>\%
  ntbt_adonis(dune ~ Management*A1)
dune.env \%>\%
  ntbt_adonis2(dune ~ Management*A1)


## ntbt_bioenv: Best Subset of Environmental Variables with
##              Maximum (Rank) Correlation with Community Dissimilarities
data(varespec)
data(varechem)

## Original function to interface
bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)

## The interface puts data as first parameter
ntbt_bioenv(varechem, wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al)

## so it can be used easily in a pipeline.
varechem \%>\%
  ntbt_bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al)


## ntbt_capscale: [Partial] Distance-based Redundancy Analysis
## ntbt_dbrda:
## Original function to interface
capscale(varespec ~ N + P + K + Condition(Al), varechem,
         dist="bray")
dbrda(varespec ~ N + P + K + Condition(Al), varechem,
         dist="bray")

## The interface puts data as first parameter
ntbt_capscale(varechem, varespec ~ N + P + K + Condition(Al), 
              dist="bray")
ntbt_dbrda(varechem, varespec ~ N + P + K + Condition(Al), 
           dist="bray")

## so it can be used easily in a pipeline.
varechem \%>\%
  ntbt_capscale(varespec ~ N + P + K + Condition(Al), dist="bray")
varechem \%>\%
  ntbt_dbrda(varespec ~ N + P + K + Condition(Al), dist="bray")


## ntbt_cca: [Partial] [Constrained] Correspondence Analysis
##           and Redundancy Analysis

## Original function to interface
cca(varespec ~ Al + P*(K + Baresoil), data = varechem)

## The interface puts data as first parameter
ntbt_cca(varechem, varespec ~ Al + P*(K + Baresoil))

## so it can be used easily in a pipeline.
varechem \%>\%
  ntbt_cca(varespec ~ Al + P*(K + Baresoil))


## ntbt_gdispweight: Dispersion-based weighting of species counts
data(mite, mite.env)
## Original function to interface
gdispweight(mite ~ Shrub + WatrCont, data = mite.env)

## The interface puts data as first parameter
ntbt_gdispweight(mite.env, mite ~ Shrub + WatrCont)

## so it can be used easily in a pipeline.
mite.env \%>\%
  ntbt_gdispweight(mite ~ Shrub + WatrCont)


## ntbt_envfit: Fits an Environmental Vector or Factor onto an Ordination
ord <- cca(dune)

## Original function to interface
envfit(ord ~ Moisture + A1, dune.env, perm = 0)

## The interface puts data as first parameter
ntbt_envfit(dune.env, ord ~ Moisture + A1, perm = 0)

## so it can be used easily in a pipeline.
dune.env \%>\%
  ntbt_envfit(ord ~ Moisture + A1, perm = 0)

## ntbt_multipart: Multiplicative Diversity Partitioning
## Original function to interface
multipart(mite ~ ., levsm, index = "renyi", scales = 1, nsimul = 19)

## The interface puts data as first parameter
ntbt_multipart(levsm, mite ~ ., index = "renyi", scales = 1, nsimul = 19)

## so it can be used easily in a pipeline.
levsm \%>\%
  ntbt_multipart(mite ~ ., index = "renyi", scales = 1, nsimul = 19)


## ntbt_ordisurf: Fit and Plot Smooth Surfaces of Variables on Ordination.
vare.dist <- vegdist(varespec)
vare.mds <- monoMDS(vare.dist)

## Original function to interface
ordisurf(vare.mds ~ Baresoil, varechem, bubble = 5)

## The interface puts data as first parameter
ntbt_ordisurf(varechem, vare.mds ~ Baresoil, bubble = 5)

## so it can be used easily in a pipeline.
varechem \%>\%
  ntbt_ordisurf(vare.mds ~ Baresoil, bubble = 5)


## ntbt_ordixyplot: Trellis (Lattice) Plots for Ordination
## Original function to interface
ordicloud(ord, form = CA2 ~ CA3*CA1, groups = Manure, data = dune.env)
ordisplom(ord, data = dune.env, form = ~ . | Management, groups=Manure)
ordixyplot(ord, data=dune.env, form = CA1 ~ CA2 | Management, groups=Manure)

## The interface puts data as first parameter
ntbt_ordicloud(dune.env, ord, form = CA2 ~ CA3*CA1, groups = Manure)
ntbt_ordisplom(dune.env, ord, form = ~ . | Management, groups=Manure)
ntbt_ordixyplot(dune.env, ord, form = CA1 ~ CA2 | Management, groups=Manure)

## so it can be used easily in a pipeline.
dune.env \%>\%
  ntbt_ordicloud(ord, form = CA2 ~ CA3*CA1, groups = Manure)
dune.env \%>\%
  ntbt_ordisplom(ord, form = ~ . | Management, groups=Manure)
dune.env \%>\%
  ntbt_ordixyplot(ord, form = CA1 ~ CA2 | Management, groups=Manure)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{vegan}
\keyword{adipart}
\keyword{adonis}
\keyword{adonis2}
\keyword{bioenv}
\keyword{capscale}
\keyword{cca}
\keyword{dbrda}
\keyword{envfit}
\keyword{gdispweight}
\keyword{multipart}
\keyword{ordicloud}
\keyword{ordisplom}
\keyword{ordisurf}
\keyword{ordixyplot}
