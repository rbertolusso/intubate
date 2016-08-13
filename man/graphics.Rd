\name{graphics}
\alias{ntbt_boxplot}
\alias{ntbt_cdplot}
\alias{ntbt_coplot}
\alias{ntbt_mosaicplot}
\alias{ntbt_pairs}
\alias{ntbt_plot}
\alias{ntbt_spineplot}
\alias{ntbt_sunflowerplot}
\alias{ntbt_stripchart}
\alias{ntbt_text}
\title{Interfaces for graphics package for data science pipelines.}
\description{
  Interfaces to \code{graphics} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_boxplot(data, ...)
ntbt_cdplot(data, ...)
ntbt_coplot(data, ...)
ntbt_mosaicplot(data, ...)
ntbt_pairs(data, ...)
ntbt_plot(data, ...)
ntbt_spineplot(data, ...)
ntbt_sunflowerplot(data, ...)
ntbt_stripchart(data, ...)
ntbt_text(data, ...)
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
\seealso{
  \code{\link{boxplot}}
}
\examples{
\dontrun{
library(intubate)
library(magrittr)

## boxplot
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

## cdplot
## NASA space shuttle o-ring failures
oring <- data.frame(
  fail = factor(c(2, 2, 2, 2, 1, 1, 1, 1,
                  1, 1, 2, 1, 2, 1, 1, 1,
                  1, 2, 1, 1, 1, 1, 1),
               levels = 1:2, labels = c("no", "yes")),
  temperature = c(53, 57, 58, 63, 66, 67, 67, 67,
                 68, 69, 70, 70, 70, 70, 72, 73,
                 75, 75, 76, 76, 78, 79, 81))

## Original function to interface
cdplot(fail ~ temperature, oring)
cdplot(fail ~ temperature, oring, bw = 2)
cdplot(fail ~ temperature, oring, bw = "SJ")

## The interface reverses the order of data and formula
ntbt_cdplot(oring, fail ~ temperature)
ntbt_cdplot(oring, fail ~ temperature, bw = 2)
ntbt_cdplot(oring, fail ~ temperature, bw = "SJ")

## so it can be used easily in a pipeline.
oring \%>\%
  ntbt_cdplot(fail ~ temperature)
oring \%>\%
  ntbt_cdplot(fail ~ temperature, bw = 2)
oring \%>\%
  ntbt_cdplot(fail ~ temperature, bw = "SJ")

## coplot
Index <- seq(length = nrow(warpbreaks))

## Original function to interface
coplot(breaks ~ Index | wool * tension, data = warpbreaks,
       show.given = 0:1)
coplot(breaks ~ Index | wool * tension, data = warpbreaks,
       col = "red", bg = "pink", pch = 21,
       bar.bg = c(fac = "light blue"))

## The interface reverses the order of data and formula
ntbt_coplot(data = warpbreaks, breaks ~ Index | wool * tension,
            show.given = 0:1)
ntbt_coplot(data = warpbreaks, breaks ~ Index | wool * tension,
            col = "red", bg = "pink", pch = 21,
            bar.bg = c(fac = "light blue"))

## so it can be used easily in a pipeline.
warpbreaks \%T>\%  ## Note the tee operator.
  ntbt_coplot(breaks ~ Index | wool * tension,
              show.given = 0:1) \%>\%
  ntbt_coplot(breaks ~ Index | wool * tension,
              col = "red", bg = "pink", pch = 21,
              bar.bg = c(fac = "light blue"))

## mosaicplot
## Original function to interface
mosaicplot(~ Sex + Age + Survived, data = Titanic, color = TRUE)

## The interface reverses the order of data and formula
ntbt_mosaicplot(data = Titanic, ~ Sex + Age + Survived, color = TRUE)

## so it can be used easily in a pipeline.
Titanic \%>\%
  ntbt_mosaicplot(~ Sex + Age + Survived, color = TRUE)

## pairs
## Original function to interface
pairs(~ Fertility + Education + Catholic, data = swiss,
      subset = Education < 20,
      main = "Swiss data, Education < 20")

## The interface reverses the order of data and formula
ntbt_pairs(data = swiss, ~ Fertility + Education + Catholic,
           subset = Education < 20,
           main = "Swiss data, Education < 20")

## so it can be used easily in a pipeline.
swiss \%>\%
  ntbt_pairs(~ Fertility + Education + Catholic,
             subset = Education < 20,
             main = "Swiss data, Education < 20")

## plot
## Original function to interface
plot(Ozone ~ Wind, data = airquality, pch = as.character(Month))

## The interface reverses the order of data and formula
ntbt_plot(data = airquality, Ozone ~ Wind, pch = as.character(Month))

## so it can be used easily in a pipeline.
airquality \%>\%
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month))
  
op <- par(mfrow = c(2,1))
airquality \%>\%  ## Note: we are *not* using the tee operator.
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month)) \%>\%
  ntbt_plot(Ozone ~ Wind, pch = as.character(Month),
            subset = Month != 7) \%>\%
  head()          ## Yes! We still have the data to do what we want!
par(op)

## text.formula() can be very natural:
within(warpbreaks, {
    time <- seq_along(breaks)
    W.T <- wool:tension
}) \%>\%
  ntbt_plot(breaks ~ time, type = "b") \%>\%
  ntbt_text(breaks ~ time, label = W.T, col = 1 + as.integer(wool))

## splineplot
## NASA space shuttle o-ring failures
oring <- data.frame(
  fail = factor(c(2, 2, 2, 2, 1, 1, 1, 1,
                  1, 1, 2, 1, 2, 1, 1, 1,
                  1, 2, 1, 1, 1, 1, 1),
               levels = 1:2, labels = c("no", "yes")),
  temperature = c(53, 57, 58, 63, 66, 67, 67, 67,
                 68, 69, 70, 70, 70, 70, 72, 73,
                 75, 75, 76, 76, 78, 79, 81))

## Original function to interface
spineplot(fail ~ temperature, oring)
spineplot(fail ~ temperature, oring, breaks = 3)

## The interface reverses the order of data and formula
ntbt_spineplot(oring, fail ~ temperature)
ntbt_spineplot(oring, fail ~ temperature, breaks = 3)

## so it can be used easily in a pipeline.
oring \%>\%
  ntbt_spineplot(fail ~ temperature)
oring \%>\%
  ntbt_spineplot(fail ~ temperature, breaks = 3)

## sunflowerplot
## Original function to interface
sunflowerplot(Petal.Width ~ Petal.Length, data = iris,
              cex = .2, cex.fact = 1, size = .035,
              seg.lwd = .8)

## The interface reverses the order of data and formula
ntbt_sunflowerplot(data = iris, Petal.Width ~ Petal.Length,
                   cex = .2, cex.fact = 1, size = .035,
                   seg.lwd = .8)

## so it can be used easily in a pipeline.
iris \%>\%
  ntbt_sunflowerplot(Petal.Width ~ Petal.Length,
                     cex = .2, cex.fact = 1, size = .035,
                     seg.lwd = .8)

## stripchart
## Original function to interface
stripchart(decrease ~ treatment, data = OrchardSprays,
           main = "stripchart(OrchardSprays)",
           vertical = TRUE, log = "y")

## The interface reverses the order of data and formula
ntbt_stripchart(data = OrchardSprays, decrease ~ treatment,
                main = "stripchart(OrchardSprays)",
                vertical = TRUE, log = "y")

## so it can be used easily in a pipeline.
OrchardSprays \%>\%
  ntbt_stripchart(decrease ~ treatment,
                  main = "stripchart(OrchardSprays)",
                  vertical = TRUE, log = "y")

## text
data <- within(warpbreaks, {
    time <- seq_along(breaks)
    W.T <- wool:tension
})

## Original function to interface
plot(breaks ~ time, data, type = "b")
text(breaks ~ time, data, label = W.T, col = 1 + as.integer(wool))

## The interface reverses the order of data and formula
ntbt_plot(data, breaks ~ time, type = "b")
ntbt_text(data, breaks ~ time, label = W.T, col = 1 + as.integer(wool))

## so it can be used easily in a pipeline.
data \%>\%
  ntbt_plot(breaks ~ time, type = "b") \%>\%
  ntbt_text(breaks ~ time, label = W.T, col = 1 + as.integer(wool)) \%>\%
  head()          ## Yes! We still have the data to do what we want!
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{graphics}
\keyword{boxplot}
\keyword{cdplot}
\keyword{coplot}
\keyword{mosaicplot}
\keyword{pairs}
\keyword{plot}
\keyword{spineplot}
\keyword{sunflowerplot}
\keyword{stripchart}
\keyword{text}
