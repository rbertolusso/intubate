\name{ntbt_contourplot}
\alias{ntbt_contourplot}
\title{Interface to contourplot for data science pipelines.}
\description{
  \code{ntbt_contourplot} is an interface to \code{lattice::\link{contourplot}}
  (Contour Plots) that can be used
  in a pipeline implemented by \code{magrittr}.}
\usage{
ntbt_contourplot(data, ...)
}
\arguments{
  \item{data}{an data frame, tibble, list (or object coercible by as.data.frame to a data frame) containing the variables in the model.}
  \item{...}{Other arguments passed to \code{\link{contourplot}}.}
}
\details{
  ntbt_contourplot calls \code{contourplot}.
}
\value{
  Object returned by \code{\link{contourplot}}.
}
\author{
Roberto Bertolusso
}
\seealso{
  \code{\link{contourplot}}
}
\examples{
library(lattice)

grid <- with(
  environmental,
  {
    ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
                   parametric = c("radiation", "wind"), span = 1, degree = 2)
    w.marginal <- seq(min(wind), max(wind), length.out = 50)
    t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
    r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
    wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
                         radiation = r.marginal)
    ret <- expand.grid(wtr.marginal)
    ret[, "fit"] <- c(predict(ozo.m, ret))
    ret
  })

## Original function to interface
contourplot(fit ~ wind * temperature | radiation, data = grid,
            cuts = 10, region = TRUE,
            xlab = "Wind Speed (mph)",
            ylab = "Temperature (F)",
            main = "Cube Root Ozone (cube root ppb)")

## The interface reverses the order of data and formula
ntbt_contourplot(data = grid, fit ~ wind * temperature | radiation,
                 cuts = 10, region = TRUE,
                 xlab = "Wind Speed (mph)",
                 ylab = "Temperature (F)",
                 main = "Cube Root Ozone (cube root ppb)")

## so it can be used easily in a pipeline.
library(magrittr)
grid \%>\%
  ntbt_contourplot(fit ~ wind * temperature | radiation,
                   cuts = 10, region = TRUE,
                   xlab = "Wind Speed (mph)",
                   ylab = "Temperature (F)",
                   main = "Cube Root Ozone (cube root ppb)")
}
\keyword{intubate}
\keyword{magrittr}
\keyword{ntbt_contourplot}
\keyword{contourplot}
