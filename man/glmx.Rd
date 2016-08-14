\name{glmx}
\alias{ntbt_glmx}
\alias{ntbt_hetglm}
\title{Interfaces for glmx package for data science pipelines.}
\description{
  Interfaces to \code{glmx} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_glmx(data, ...)
ntbt_hetglm(data, ...)
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
}
}
\keyword{intubate}
\keyword{magrittr}
\keyword{glmx}
\keyword{hetglm}
