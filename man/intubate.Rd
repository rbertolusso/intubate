\name{intubate}
\alias{intubate}
\alias{intubate_rm_all_interfaces}
\alias{ntbt}
\alias{ntbt_function_formula_data}
\alias{ntbt_function_model_data}
\alias{ntbt_function_object_data}
\alias{ntbt_function_x_data}
\title{1) Interfaces "on demand" and 2) calling of non-pipe-aware functions directly.}
\description{
  \code{intubate} is a helper function used to implement "on demand" interfaces
  to functions you want to use in pipelines implemented by \code{magrittr}.
  Suppose intubate does not implement an interface to a fantasy function
  (such as \code{fantasy(formula, data, ...)}) that is non-pipe-aware (because
  \code{data} is not its first parameter).
  To create an interface
  you only need the line of code  \code{ntbt_fantasy <- intubate}, 
  after which \code{ntbt_fantasy(data, ...)} can be used in a data
  science pipeline. See examples for details and discussion.
  
  \code{ntbt} lets you interface the non-pipe-aware function directly
  without creating an interface.}
\usage{
## Helper function to define interfaces
intubate(data, ...)

## Remove all interfaces (functions starting with ntbt_)
intubate_rm_all_interfaces()

## Function to call non-pipe-aware functions directly
ntbt(data, fti, ...)

## Deprecated helper functions
## (for compatibility with 0.99.2. Will be removed at some point)
ntbt_function_formula_data(data, ...)
ntbt_function_model_data(data, ...)
ntbt_function_object_data(data, ...)
ntbt_function_x_data(data, ...)
}
\arguments{
  \item{data}{data frame, tibble, list, ...}
  \item{fti}{function to interface.}
  \item{...}{Other arguments passed to interfaced function.}
}
\value{
  Object returned by interfaced function. If you call \code{intubate} directly
  it will fail.
}
\author{
Roberto Bertolusso
}
\examples{
\dontrun{
library(intubate)

## NOTE: intubate implements an interface to
##       *xyplot* (in package lattice), called *ntbt_xyplot*.
##       For the sake of argument, let's suppose the
##       interface does not exist, and you want to implement
##       it "on demand" to use it in a pipeline.

## Original function you would like to interface
library(lattice)
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## If you try to use *xyplot* directly in a data pipeline
## it will raise an error
library(magrittr)
try(iris \%>\%
    xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
           scales = "free", layout = c(2, 2),
           auto.key = list(x = .6, y = .7, corner = c(0, 0))),
    silent = TRUE)
geterrmessage()

## The error disappears if you create an interface to *xyplot*.

## Step needed to create an interface to *xyplot*.

ntbt_xyplot <- intubate

## NOTE: names of interfaces must start with
##       *ntbt_* followed by the name of the function
##       (*xyplot* in this case) you want to interface.

## Now you can use the interface instead of the original
## function. Just remember to switch the order of
## *data* and *x* (there is no need to name the parameters).
ntbt_xyplot(iris, 
            Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
            scales = "free", layout = c(2, 2),
            auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## The newly created interface can be used easily in a pipeline.
library(magrittr)
iris \%>\%
  ntbt_xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
              scales = "free", layout = c(2, 2),
              auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## Alternative: call non-pipe-aware function directly.
## You can also avoid creating an interface, by calling ntbt with the name of
## the function to interface.
ntbt(iris, xyplot,
     Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
     scales = "free", layout = c(2, 2),
     auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## In a pipeline
iris \%>\%
  ntbt(xyplot, Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt}
