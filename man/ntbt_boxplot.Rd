\name{ntbt_boxplot}
\alias{ntbt_boxplot}
\title{Interface to boxplot for data science pipelines.}
\description{
  \code{ntbt_boxplot} is an interface to \code{graphics::\link{boxplot}}
  (Box Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_boxplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{boxplot}}.}
}
\details{
  ntbt_boxplot calls \code{boxplot}.
}
\value{
  Object returned by \code{\link{boxplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{boxplot}}
}
\examples{
## Original function to interface
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

## The interface reverses the order of data and formula
ntbt_boxplot(data = InsectSprays, count ~ spray, col = "lightgray")

ntbt_boxplot(data = ToothGrowth, len ~ dose,
             boxwex = 0.25, at = 1:3 - 0.2,
             subset = supp == "VC", col = "yellow",
             main = "Guinea Pigs' Tooth Growth",
             xlab = "Vitamin C dose mg",
             ylab = "tooth length",
             xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
ntbt_boxplot(data = ToothGrowth, len ~ dose,
             add = TRUE,
             boxwex = 0.25, at = 1:3 + 0.2,
             subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

## so it can be used easily in a pipeline.
library(magrittr)
InsectSprays \%>\%
  ntbt_boxplot(count ~ spray, col = "lightgray")

ToothGrowth \%T>\%  ## Note the tee operator.
  ntbt_boxplot(len ~ dose,
               boxwex = 0.25, at = 1:3 - 0.2,
               subset = supp == "VC", col = "yellow",
               main = "Guinea Pigs' Tooth Growth",
               xlab = "Vitamin C dose mg",
               ylab = "tooth length",
               xlim = c(0.5, 3.5), ylim = c(0, 35),
               yaxs = "i") \%>\%
  ntbt_boxplot(len ~ dose,
               add = TRUE,
               boxwex = 0.25, at = 1:3 + 0.2,
               subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_boxplot}
\keyword{boxplot}
