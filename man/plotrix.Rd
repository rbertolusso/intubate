\name{plotrix}
\alias{ntbt_barNest}
\alias{ntbt_brkdn.plot}
\alias{ntbt_brkdnNest}
\alias{ntbt_histStack}
\alias{ntbt_plotH}
\title{Interfaces for plotrix package for data science pipelines.}
\description{
  Interfaces to \code{plotrix} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_barNest(data, ...)
ntbt_brkdn.plot(data, ...)
ntbt_brkdnNest(data, ...)
ntbt_histStack(data, ...)
ntbt_plotH(data, ...)
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
library(plotrix)

## ntbt_barNest: Display a nested breakdown of numeric values
titanic<-data.frame(
  class=c(rep("1st",325),rep("2nd",285),rep("3rd",706),rep("Crew",885)),
  age=c(rep("Adult",319),rep("Child",6),rep("Adult",261),rep("Child",24),
        rep("Adult",627),rep("Child",79),rep("Adult",885)),
  sex=c(rep("M",175),rep("F",144),rep("M",5),rep("F",1),
        rep("M",168),rep("F",93),rep("M",11),rep("F",13),
        rep("M",462),rep("F",165),rep("M",48),rep("F",31),
        rep("M",862),rep("F",23)),
  survived=c(rep("Yes",57),rep("No",118),rep("Yes",140),rep("No",4),rep("Yes",6),
             rep("Yes",14),rep("No",154),rep("Yes",80),rep("No",13),rep("Yes",24),
             rep("Yes",75),rep("No",387),rep("Yes",76),rep("No",89),
             rep("Yes",13),rep("No",35),rep("Yes",14),rep("No",17),
             rep("Yes",192),rep("No",670),rep("Yes",20),rep("No",3)))
titanic.colors<-list("gray90",c("#0000ff","#7700ee","#aa00cc","#dd00aa"),
                     c("#ddcc00","#ee9900"),c("pink","lightblue"))

## Original function to interface
barNest(survived ~ class + age + sex, titanic, col = titanic.colors,
        showall = TRUE, main = "Titanic survival by class, age and sex",
        ylab = "Proportion surviving", FUN = c("propbrk","binciWu","binciWl","valid.n"),
        shrink = 0.15, trueval = "Yes")

## The interface puts data as first parameter
ntbt_barNest(titanic, survived ~ class + age + sex, col = titanic.colors,
             showall = TRUE, main = "Titanic survival by class, age and sex",
             ylab = "Proportion surviving", FUN = c("propbrk","binciWu","binciWl","valid.n"),
             shrink = 0.15, trueval = "Yes")

## so it can be used easily in a pipeline.
titanic \%>\%
  ntbt_barNest(survived ~ class + age + sex, col = titanic.colors,
               showall = TRUE, main = "Titanic survival by class, age and sex",
               ylab = "Proportion surviving", FUN = c("propbrk","binciWu","binciWl","valid.n"),
               shrink = 0.15, trueval = "Yes")


## ntbt_brkdn.plot: A point/line plotting routine
test.df<-data.frame(a=rnorm(80)+4,b=rnorm(80)+4,c=rep(LETTERS[1:4],each=20),
                    d=rep(rep(letters[1:4],each=4),5))
## Original function to interface
brkdn.plot("a", "c", "d", test.df, pch = 1:4, col = 1:4)

## The interface puts data as first parameter
ntbt_brkdn.plot(test.df, "a", "c", "d", pch = 1:4, col = 1:4)

## so it can be used easily in a pipeline.
test.df \%>\%
  ntbt_brkdn.plot("a", "c", "d", pch = 1:4, col = 1:4)


## ntbt_brkdnNest: Perform a nested breakdown of numeric values
brkdntest <- data.frame(Age=rnorm(100,25,10),
                        Sex=sample(c("M","F"),100,TRUE),
                        Marital=sample(c("M","X","S","W"),100,TRUE),
                        Employ=sample(c("FT","PT","NO"),100,TRUE))
## Original function to interface
brkdnNest(Age ~ Sex + Marital + Employ, data = brkdntest)

## The interface puts data as first parameter
ntbt_brkdnNest(brkdntest, Age ~ Sex + Marital + Employ)

## so it can be used easily in a pipeline.
brkdntest \%>\%
  ntbt_brkdnNest(Age ~ Sex + Marital + Employ)


## ntbt_histStack: Histogram "stacked" by categories
set.seed(409)
df <- data.frame(len=rnorm(100)+5,
                 grp=sample(c("A","B","C","D"),100,replace=TRUE))

## Original function to interface
histStack(len ~ grp, data = df, main = "Default (rainbow) colors",
          xlab = "Length category")

## The interface puts data as first parameter
ntbt_histStack(df, len ~ grp, main = "Default (rainbow) colors",
               xlab = "Length category")

## so it can be used easily in a pipeline.
df \%>\%
  ntbt_histStack(len ~ grp, main = "Default (rainbow) colors",
                 xlab = "Length category")


## ntbt_plotH: Scatterplot with histogram-like bars
d <- data.frame(x=c(1,5,10:20),y=runif(13)+1,
                g=factor(sample(c("A","B","C"),13,replace=TRUE)))

## Original function to interface
plotH(y ~ x, data = d)

## The interface puts data as first parameter
ntbt_plotH(d, y ~ x)

## so it can be used easily in a pipeline.
d \%>\%
  ntbt_plotH(y ~ x)
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{plotrix}
\keyword{barNest}
\keyword{brkdn.plot}
\keyword{brkdnNest}
\keyword{histStack}
\keyword{plotH}
