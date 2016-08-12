\name{kernlab}
\alias{ntbt_gausspr}
\alias{ntbt_kfa}
\alias{ntbt_kha}
\alias{ntbt_kkmeans}
\alias{ntbt_kpca}
\alias{ntbt_kqr}
\alias{ntbt_ksvm}
\alias{ntbt_lssvm}
\alias{ntbt_rvm}
\alias{ntbt_sigest}
\alias{ntbt_specc}
\title{Interfaces for kernlab package for data science pipelines.}
\description{
  Interfaces to \code{kernlab} functions that can be used
  in a pipeline implemented by \code{magrittr}.
}
\usage{
ntbt_gausspr(data, ...)
ntbt_kfa(data, ...)
ntbt_kha(data, ...)
ntbt_kkmeans(data, ...)
ntbt_kpca(data, ...)
ntbt_kqr(data, ...)
ntbt_ksvm(data, ...)
ntbt_lssvm(data, ...)
ntbt_rvm(data, ...)
ntbt_sigest(data, ...)
ntbt_specc(data, ...)
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
\keyword{kernlab}
\keyword{gausspr}
\keyword{kfa}
\keyword{kha}
\keyword{kkmeans}
\keyword{kpca}
\keyword{kqr}
\keyword{ksvm}
\keyword{lssvm}
\keyword{rvm}
\keyword{sigest}
\keyword{specc}
