% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mecklenberg}
\alias{mecklenberg}
\title{Mecklenberg PSA Reform Data}
\format{
A data frame with 54 rows and 10 variables:
\describe{
  \item{\code{month}}{integer Month, with 0 being month of policy implementation.}
  \item{\code{karr}}{integer Total count of arrests.}
  \item{\code{pbail}}{double Proportion of cases in a given month assigned bail (or outright detention).}
  \item{\code{pptrel}}{double Proportion of cases assigned to pretrial supervised release.}
  \item{\code{pror}}{double Proportion of cases released on own recognizance.}
  \item{\code{pb4c}}{double Proportion of cases assigned to money bail (alternate coding from pbail, above).}
  \item{\code{avg_days_initial}}{double Average number of days spent detained before release due to bail, case resolution, etc.}
  \item{\code{avg_t2d}}{double Average time to case resolution (in days).}
  \item{\code{pstint7}}{double Proportion detained longer than 7 days.}
  \item{\code{pstint30}}{double Proportion detained longer than 30 days.}
}
}
\usage{
mecklenberg
}
\description{
Monthly aggregate outcomes of various measures of interest from Mecklenberg. See MDRC Report.
}
\keyword{datasets}
