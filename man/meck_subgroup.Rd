% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{meck_subgroup}
\alias{meck_subgroup}
\title{Mecklenberg data by subgroup of charge type}
\format{
A data frame with 144 rows and 5 variables:
\describe{
  \item{\code{month}}{integer Month, with 0 being month of policy implementation.}
  \item{\code{n.cases}}{integer Number of cases of that subgroup for that month}
  \item{\code{n.bail}}{interger Total number of cases given bail for that subgroup for that month}
  \item{\code{pbail}}{double Proportion of new cases in given subgroup in that month assigned bail} 
  \item{\code{category}}{character Category of group (charge type).}
}
}
\usage{
meck_subgroup
}
\description{
Mecklenberg data that gives proportion of different charge categories of cases given bail (by month).
}
\keyword{datasets}
