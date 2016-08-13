\name{lmtest}
\alias{ntbt_bgtest}
\alias{ntbt_bptest}
\alias{ntbt_coxtest}
\alias{ntbt_dwtest}
\alias{ntbt_encomptest}
\alias{ntbt_gqtest}
\alias{ntbt_grangertest}
\alias{ntbt_harvtest}
\alias{ntbt_hmctest}
\alias{ntbt_jtest}
\alias{ntbt_raintest}
\alias{ntbt_resettest}
\title{Interfaces for lmtest package for data science pipelines.}
\description{
  Interfaces to \code{lmtest} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_bgtest(data, ...)
ntbt_bptest(data, ...)
ntbt_coxtest(data, ...)
ntbt_dwtest(data, ...)
ntbt_encomptest(data, ...)
ntbt_gqtest(data, ...)
ntbt_grangertest(data, ...)
ntbt_harvtest(data, ...)
ntbt_hmctest(data, ...)
ntbt_jtest(data, ...)
ntbt_raintest(data, ...)
ntbt_resettest(data, ...)
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
library(lmtest)

## ntbt_bgtest: Breusch-Godfrey Test for higher-order serial correlation
x <- rep(c(1, -1), 50)
y1 <- 1 + x + rnorm(100)
dta <- data.frame(x, y1)

## or for fourth-order serial correlation
## Original function to interface
bgtest(y1 ~ x, order = 4, data = dta)

## The interface puts data as first parameter
ntbt_bgtest(dta, y1 ~ x, order = 4)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_bgtest(y1 ~ x, order = 4)


## ntbt_bptest: Breusch-Pagan test against heteroskedasticity
## ntbt_gqtest: Goldfeld-Quandt test against heteroskedasticity
## ntbt_hmctest: Harrison-McCabe test for heteroskedasticity
x <- rep(c(-1,1), 50)
err1 <- c(rnorm(50, sd=1), rnorm(50, sd=2))
err2 <- rnorm(100)
y1 <- 1 + x + err1
y2 <- 1 + x + err2
dtah <- data.frame(x, y1, y2)

## Original function to interface
bptest(y1 ~ x, data = dtah)
gqtest(y1 ~ x, data = dtah)
hmctest(y1 ~ x, data = dtah)
bptest(y2 ~ x, data = dtah)
gqtest(y2 ~ x, data = dtah)
hmctest(y2 ~ x, data = dtah)

## The interface puts data as first parameter
ntbt_bptest(dtah, y1 ~ x)
ntbt_gqtest(dtah, y1 ~ x)
ntbt_hmctest(dtah, y1 ~ x)
ntbt_bptest(dtah, y2 ~ x)
ntbt_gqtest(dtah, y2 ~ x)
ntbt_hmctest(dtah, y2 ~ x)

## so it can be used easily in a pipeline.
dtah \%>\%
  ntbt_bptest(y1 ~ x)
dtah \%>\%
  ntbt_gqtest(y1 ~ x)
dtah \%>\%
  ntbt_hmctest(y1 ~ x)
dtah \%>\%
  ntbt_bptest(y2 ~ x)
dtah \%>\%
  ntbt_gqtest(y2 ~ x)
dtah \%>\%
  ntbt_hmctest(y2 ~ x)


## ntbt_coxtest: Cox Test for Comparing Non-Nested Models
## ntbt_encomptest: encompassing test of Davidson & MacKinnon for comparing non-nested models
## ntbt_jtest: Davidson-MacKinnon J test for comparing non-nested models
data(USDistLag)
usdl <- na.contiguous(cbind(USDistLag, lag(USDistLag, k = -1)))
colnames(usdl) <- c("con", "gnp", "con1", "gnp1")

## Original function to interface
coxtest(con ~ gnp + con1, con ~ gnp + gnp1, data = usdl)
encomptest(con ~ gnp + con1, con ~ gnp + gnp1, data = usdl)
jtest(con ~ gnp + con1, con ~ gnp + gnp1, data = usdl)

## The interface puts data as first parameter
ntbt_coxtest(usdl, con ~ gnp + con1, con ~ gnp + gnp1)
ntbt_encomptest(usdl, con ~ gnp + con1, con ~ gnp + gnp1)
ntbt_jtest(usdl, con ~ gnp + con1, con ~ gnp + gnp1)

## so it can be used easily in a pipeline.
usdl \%>\%
  ntbt_coxtest(con ~ gnp + con1, con ~ gnp + gnp1)
usdl \%>\%
  ntbt_encomptest(con ~ gnp + con1, con ~ gnp + gnp1)
usdl \%>\%
  ntbt_jtest(con ~ gnp + con1, con ~ gnp + gnp1)

## ntbt_dwtest: Durbin-Watson test for autocorrelation of disturbances
err1 <- rnorm(100)
x <- rep(c(-1,1), 50)
y1 <- 1 + x + err1
err2 <- filter(err1, 0.9, method="recursive")
y2 <- 1 + x + err2
dta <- data.frame(y1, y2, x)

## Original function to interface
dwtest(y1 ~ x, data = dta)
dwtest(y2 ~ x, data = dta)

## The interface puts data as first parameter
ntbt_dwtest(dta, y1 ~ x)
ntbt_dwtest(dta, y2 ~ x)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_dwtest(y1 ~ x)
dta \%>\%
  ntbt_dwtest(y2 ~ x)


## ntbt_grangertest: Test for Granger Causality
data(ChickEgg)
## Original function to interface
grangertest(egg ~ chicken, order = 3, data = ChickEgg)
grangertest(chicken ~ egg, order = 3, data = ChickEgg)

## The interface puts data as first parameter
ntbt_grangertest(ChickEgg, egg ~ chicken, order = 3)
ntbt_grangertest(ChickEgg, chicken ~ egg, order = 3)

## so it can be used easily in a pipeline.
ChickEgg \%>\%
  ntbt_grangertest(egg ~ chicken, order = 3)
ChickEgg \%>\%
  ntbt_grangertest(chicken ~ egg, order = 3)


## ntbt_harvtest: Harvey-Collier test for linearity
x <- 1:50
y1 <- 1 + x + rnorm(50)
y2 <- y1 + 0.3*x^2
dta <- data.frame(y1, x)

## Original function to interface
harvtest(y1 ~ x, data = dta)

## The interface puts data as first parameter
ntbt_harvtest(dta, y1 ~ x)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_harvtest(y1 ~ x)

## ntbt_raintest: Rainbow test for linearity
x <- c(1:30)
y <- x^2 + rnorm(30,0,2)
dta <- data.frame(x, y)

## Original function to interface
raintest(y ~ x, data = dta)

## The interface puts data as first parameter
ntbt_raintest(dta, y ~ x)

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_raintest(y ~ x)


## ntbt_resettest: Ramsey's RESET test for functional form
x <- c(1:30)
y1 <- 1 + x + x^2 + rnorm(30)
y2 <- 1 + x + rnorm(30)
dta <- data.frame(x, y1, y2)

## Original function to interface
resettest(y1 ~ x , power=2, type="regressor", data = dta)
resettest(y2 ~ x , power=2, type="regressor", data = dta)

## The interface puts data as first parameter
ntbt_resettest(dta, y1 ~ x , power=2, type="regressor")
ntbt_resettest(dta, y2 ~ x , power=2, type="regressor")

## so it can be used easily in a pipeline.
dta \%>\%
  ntbt_resettest(y1 ~ x , power=2, type="regressor")
dta \%>\%
  ntbt_resettest(y2 ~ x , power=2, type="regressor")
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{lmtest}
\keyword{bgtest}
\keyword{bptest}
\keyword{coxtest}
\keyword{dwtest}
\keyword{encomptest}
\keyword{gqtest}
\keyword{grangertest}
\keyword{harvtest}
\keyword{hmctest}
\keyword{jtest}
\keyword{raintest}
\keyword{resettest}
