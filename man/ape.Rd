\name{ape}
\alias{ntbt_binaryPGLMM}
\alias{ntbt_compar.gee}
\alias{ntbt_correlogram.formula}
\alias{ntbt_lmorigin}
\alias{ntbt_yule.cov}
\title{Interfaces for ape package for data science pipelines.}
\description{
  Interfaces to \code{ape} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_binaryPGLMM(data, ...)
ntbt_compar.gee(data, ...)
ntbt_correlogram.formula(data, ...)
ntbt_lmorigin(data, ...)
ntbt_yule.cov(data, ...)
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
library(ape)

## ntbt_binaryPGLMM: Phylogenetic Generalized Linear Mixed Model for Binary Data
n <- 100
phy <- compute.brlen(rtree(n=n), method = "Grafen", power = 1)
X1 <- rTraitCont(phy, model = "BM", sigma = 1)
X1 <- (X1 - mean(X1))/var(X1)
sim.dat <- data.frame(Y=array(0, dim=n), X1=X1, row.names=phy$tip.label)
sim.dat$Y <- binaryPGLMM.sim(Y ~ X1, phy = phy, data = sim.dat, s2 = .5,
                             B = matrix(c(0, .25), nrow = 2, ncol = 1), nrep = 1)$Y

## Original function to interface
binaryPGLMM(Y ~ X1, phy = phy, data = sim.dat)

## The interface puts data as first parameter
ntbt_binaryPGLMM(sim.dat, Y ~ X1, phy = phy)

## so it can be used easily in a pipeline.
sim.dat \%>\%
  ntbt_binaryPGLMM(Y ~ X1, phy = phy)


## ntbt_compar.gee: Comparative Analysis with GEEs
tr <- "((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galago:1.00);"
tree.primates <- read.tree(text = tr)
dta <- data.frame(X = c(4.09434, 3.61092, 2.37024, 2.02815, -1.46968),
                  Y = c(4.74493, 3.33220, 3.36730, 2.89037, 2.30259))
rownames(dta) <- tree.primates$tip.label

## Original function to interface
compar.gee(X ~ Y, phy = tree.primates, data = dta)

## The interface puts data as first parameter
ntbt_compar.gee(dta, X ~ Y, phy = tree.primates)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_compar.gee(X ~ Y, phy = tree.primates)


## ntbt_correlogram.formula: Phylogenetic Correlogram
data(carnivora)

## Original function to interface
correlogram.formula(SW ~ Order/SuperFamily/Family/Genus,
                    data = carnivora)

## The interface puts data as first parameter
ntbt_correlogram.formula(carnivora, SW ~ Order/SuperFamily/Family/Genus)

## so it can be used easily in a pipeline.
carnivora \%>\%
  ntbt_correlogram.formula(SW ~ Order/SuperFamily/Family/Genus)


## ntbt_lmorigin: Multiple regression through the origin
data(lmorigin.ex1)

## Original function to interface
lmorigin(SO2 ~ ., data = lmorigin.ex1, origin = FALSE, nperm = 99)

## The interface puts data as first parameter
ntbt_lmorigin(lmorigin.ex1, SO2 ~ ., origin = FALSE, nperm = 99)

## so it can be used easily in a pipeline.
lmorigin.ex1 \%>\%
  ntbt_lmorigin(SO2 ~ ., origin = FALSE, nperm = 99)

## ntbt_yule.cov: Fits the Yule Model With Covariates
data(bird.orders)
dta <- data.frame (x = rnorm(45))

## Original function to interface
yule.cov(bird.orders, ~ x, data = dta)

## The interface puts data as first parameter
ntbt_yule.cov(dta, bird.orders, ~ x)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_yule.cov(bird.orders, ~ x)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ape}
\keyword{binaryPGLMM}
\keyword{compar.gee}
\keyword{correlogram.formula}
\keyword{lmorigin}
\keyword{yule.cov}
