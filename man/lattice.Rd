\name{lattice}
\alias{ntbt_barchart}
\alias{ntbt_bwplot}
\alias{ntbt_cloud}
\alias{ntbt_contourplot}
\alias{ntbt_densityplot}
\alias{ntbt_dotplot}
\alias{ntbt_histogram}
\alias{ntbt_levelplot}
\alias{ntbt_oneway}
\alias{ntbt_parallelplot}
\alias{ntbt_qq}
\alias{ntbt_qqmath}
\alias{ntbt_splom}
\alias{ntbt_stripplot}
\alias{ntbt_tmd}
\alias{ntbt_wireframe}
\alias{ntbt_xyplot}
\title{Interfaces for lattice package for data science pipelines.}
\description{
  Interfaces to \code{lattice} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_barchart(data, ...)
ntbt_bwplot(data, ...)
ntbt_cloud(data, ...)
ntbt_contourplot(data, ...)
ntbt_densityplot(data, ...)
ntbt_dotplot(data, ...)
ntbt_histogram(data, ...)
ntbt_levelplot(data, ...)
ntbt_oneway(data, ...)
ntbt_parallelplot(data, ...)
ntbt_qq(data, ...)
ntbt_qqmath(data, ...)
ntbt_splom(data, ...)
ntbt_stripplot(data, ...)
ntbt_tmd(data, ...)
ntbt_wireframe(data, ...)
ntbt_xyplot(data, ...)
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
library(magrittr)
library(lattice)

## barchart
## Original function to interface
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))

## The interface reverses the order of data and formula
ntbt_barchart(data = barley, yield ~ variety | site,
              groups = year, layout = c(1,6), stack = TRUE,
              auto.key = list(space = "right"),
              ylab = "Barley Yield (bushels/acre)",
              scales = list(x = list(rot = 45)))

## so it can be used easily in a pipeline.
barley \%>\%
  ntbt_barchart(yield ~ variety | site,
                groups = year, layout = c(1,6), stack = TRUE,
                auto.key = list(space = "right"),
                ylab = "Barley Yield (bushels/acre)",
                scales = list(x = list(rot = 45)))

## bwplot
## Original function to interface
bwplot(voice.part ~ height, data = singer, xlab = "Height (inches)")

## The interface reverses the order of data and formula
ntbt_bwplot(data = singer, voice.part ~ height, xlab = "Height (inches)")

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_bwplot(voice.part ~ height, xlab = "Height (inches)")

## cloud
## Original function to interface
cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
      screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## The interface reverses the order of data and formula
ntbt_cloud(data = iris, Sepal.Length ~ Petal.Length * Petal.Width | Species,
           screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species,
             screen = list(x = -90, y = 70), distance = .4, zoom = .6)

## contourplot
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
grid \%>\%
  ntbt_contourplot(fit ~ wind * temperature | radiation,
                   cuts = 10, region = TRUE,
                   xlab = "Wind Speed (mph)",
                   ylab = "Temperature (F)",
                   main = "Cube Root Ozone (cube root ppb)")

## densityplot
## Original function to interface
densityplot(~ height | voice.part, data = singer, layout = c(2, 4),  
            xlab = "Height (inches)", bw = 5)

## The interface reverses the order of data and formula
ntbt_densityplot(data = singer, ~ height | voice.part, layout = c(2, 4),  
                 xlab = "Height (inches)", bw = 5)

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_densityplot(~ height | voice.part, layout = c(2, 4),  
                   xlab = "Height (inches)", bw = 5)

## dotplot
## Original function to interface
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)

## The interface reverses the order of data and formula
ntbt_dotplot(data = barley, variety ~ yield | site, groups = year,
             key = simpleKey(levels(barley$year), space = "right"),
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)

## so it can be used easily in a pipeline.
barley \%>\%
  ntbt_dotplot(variety ~ yield | site, groups = year,
             key = simpleKey(levels(barley$year), space = "right"),
             xlab = "Barley Yield (bushels/acre) ",
             aspect=0.5, layout = c(1,6), ylab=NULL)

## histogram
## Original function to interface
histogram(~ height | voice.part, data = singer,
          xlab = "Height (inches)", type = "density",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm, col = "black",
                              args = list(mean=mean(x),sd=sd(x)))
          })

## The interface reverses the order of data and formula
ntbt_histogram(data = singer, ~ height | voice.part,
               xlab = "Height (inches)", type = "density",
               panel = function(x, ...) {
                 panel.histogram(x, ...)
                 panel.mathdensity(dmath = dnorm, col = "black",
                                   args = list(mean=mean(x),sd=sd(x)))
               })

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_histogram(~ height | voice.part,
                 xlab = "Height (inches)", type = "density",
                 panel = function(x, ...) {
                   panel.histogram(x, ...)
                   panel.mathdensity(dmath = dnorm, col = "black",
                                     args = list(mean=mean(x),sd=sd(x)))
                 })

## levelplot
x <- seq(pi/4, 5 * pi, length.out = 100)
y <- seq(pi/4, 5 * pi, length.out = 100)
r <- as.vector(sqrt(outer(x^2, y^2, "+")))
grid <- expand.grid(x = x, y = y)
grid$z <- cos(r^2) * exp(-r/(pi^3))

## Original function to interface
levelplot(z ~ x*y, grid, cuts = 50, scales = list(log = "e"), xlab = "",
          ylab = "", main = "Weird Function", sub = "with log scales",
          colorkey = FALSE, region = TRUE)

## The interface reverses the order of data and formula
ntbt_levelplot(grid, z ~ x*y, cuts = 50, scales = list(log = "e"), xlab = "",
               ylab = "", main = "Weird Function", sub = "with log scales",
               colorkey = FALSE, region = TRUE)

## so it can be used easily in a pipeline.
grid \%>\%
  ntbt_levelplot(z ~ x*y, cuts = 50, scales = list(log = "e"), xlab = "",
                 ylab = "", main = "Weird Function", sub = "with log scales",
                 colorkey = FALSE, region = TRUE)

## oneway
## Original function to interface
fit <- oneway(height ~ voice.part, data = singer, spread = 1)
rfs(fit, aspect = 1)

## The interface reverses the order of data and formula
fit <- ntbt_oneway(data = singer, height ~ voice.part, spread = 1)
rfs(fit, aspect = 1)

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_oneway(height ~ voice.part, spread = 1) \%>\%
  rfs(aspect = 1)

## parallelplot
## Original function to interface
parallelplot(~iris[1:4], iris, groups = Species,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

## The interface reverses the order of data and formula
ntbt_parallelplot(iris, ~iris[1:4], groups = Species,
                  horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_parallelplot(~iris[1:4], groups = Species,
                    horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

## qq
## Original function to interface
qq(voice.part ~ height, data = singer, aspect = 1,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

## The interface reverses the order of data and formula
ntbt_qq(data = singer, voice.part ~ height, aspect = 1,
        subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_qq(voice.part ~ height, aspect = 1,
          subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

## qqmath
## Original function to interface
qqmath(~ height | voice.part, data = singer, aspect = "xy",
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })

## The interface reverses the order of data and formula
ntbt_qqmath(data = singer, ~ height | voice.part, aspect = "xy",
            prepanel = prepanel.qqmathline,
            panel = function(x, ...) {
              panel.qqmathline(x, ...)
              panel.qqmath(x, ...)
            })

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_qqmath(~ height | voice.part, aspect = "xy",
              prepanel = prepanel.qqmathline,
              panel = function(x, ...) {
                panel.qqmathline(x, ...)
                panel.qqmath(x, ...)
              })

## splom
super.sym <- trellis.par.get("superpose.symbol")

## Original function to interface
splom(~ iris[1:4], data = iris, groups = Species,
      panel = panel.superpose,
      key = list(title = "Three Varieties of Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))
splom(~ iris[1:3] | Species, data = iris, 
      layout=c(2,2), pscales = 0,
      varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
      page = function(...) {
        ltext(x = seq(.6, .8, length.out = 4), 
              y = seq(.9, .6, length.out = 4), 
              labels = c("Three", "Varieties", "of", "Iris"),
              cex = 2)
      })

## The interface reverses the order of data and formula
ntbt_splom(data = iris, ~ iris[1:4], groups = Species,
           panel = panel.superpose,
           key = list(title = "Three Varieties of Iris",
                      columns = 3, 
                      points = list(pch = super.sym$pch[1:3],
                                    col = super.sym$col[1:3]),
                      text = list(c("Setosa", "Versicolor", "Virginica"))))
ntbt_splom(data = iris, ~ iris[1:3] | Species,
           layout=c(2,2), pscales = 0,
           varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
           page = function(...) {
             ltext(x = seq(.6, .8, length.out = 4), 
                   y = seq(.9, .6, length.out = 4), 
                   labels = c("Three", "Varieties", "of", "Iris"),
                   cex = 2)
           })

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_splom(~ iris[1:4], groups = Species,
             panel = panel.superpose,
             key = list(title = "Three Varieties of Iris",
                        columns = 3, 
                        points = list(pch = super.sym$pch[1:3],
                                      col = super.sym$col[1:3]),
                        text = list(c("Setosa", "Versicolor", "Virginica"))))
iris \%>\%
  ntbt_splom(~ iris[1:3] | Species,
             layout=c(2,2), pscales = 0,
             varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
             page = function(...) {
               ltext(x = seq(.6, .8, length.out = 4), 
                     y = seq(.9, .6, length.out = 4), 
                     labels = c("Three", "Varieties", "of", "Iris"),
                     cex = 2)
             })

## stripplot
## Original function to interface
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")

## The interface reverses the order of data and formula
ntbt_stripplot(data = singer, voice.part ~ jitter(height), aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")

## so it can be used easily in a pipeline.
singer \%>\%
  ntbt_stripplot(voice.part ~ jitter(height), aspect = 1,
                 jitter.data = TRUE, xlab = "Height (inches)")

## tmd
## Original function to interface
tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
    data = iris, scales = "free", layout = c(2, 2),
    auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## The interface reverses the order of data and formula
ntbt_tmd(data = iris, 
         Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         scales = "free", layout = c(2, 2),
         auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_tmd(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
           scales = "free", layout = c(2, 2),
           auto.key = list(x = .6, y = .7, corner = c(0, 0)))


## wireframe
g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$gr + g$y^2) * g$gr)

## Original function to interface
wireframe(z ~ x * y, data = g, groups = gr,
          scales = list(arrows = FALSE),
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -60))

## The interface reverses the order of data and formula
ntbt_wireframe(data = g, z ~ x * y, groups = gr,
               scales = list(arrows = FALSE),
               drape = TRUE, colorkey = TRUE,
               screen = list(z = 30, x = -60))

## so it can be used easily in a pipeline.
g \%>\%
  ntbt_wireframe(z ~ x * y, groups = gr,
                 scales = list(arrows = FALSE),
                 drape = TRUE, colorkey = TRUE,
                 screen = list(z = 30, x = -60))

## xyplot
## Original function to interface
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## The interface reverses the order of data and formula
ntbt_xyplot(data = iris, 
            Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
            scales = "free", layout = c(2, 2),
            auto.key = list(x = .6, y = .7, corner = c(0, 0)))

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
              scales = "free", layout = c(2, 2),
              auto.key = list(x = .6, y = .7, corner = c(0, 0)))

}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{lattice}
\keyword{barchart}
\keyword{bwplot}
\keyword{cloud}
\keyword{contourplot}
\keyword{densityplot}
\keyword{dotplot}
\keyword{histogram}
\keyword{levelplot}
\keyword{oneway}
\keyword{parallelplot}
\keyword{qq}
\keyword{qqmath}
\keyword{splom}
\keyword{stripplot}
\keyword{tmd}
\keyword{wireframe}
\keyword{xyplot}
