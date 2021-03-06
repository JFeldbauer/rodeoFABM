% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gotm_tools.R
\name{get_var}
\alias{get_var}
\title{Recieve parameter from a netcdf file created by GOTM}
\usage{
get_var(
  file = "output.nc",
  var = "temp",
  z_out = NULL,
  t_out = NULL,
  res = 0.5,
  reference = "surface",
  long = FALSE
)
}
\arguments{
\item{file}{Path to the netcdf file}

\item{var}{Name of the variable to get}

\item{z_out}{Vector of depths from which to get the variable. If no depths are selected
(NULL, the default) all depths from the lowest to the highest depth in a equal distance of
'res'}

\item{t_out}{Vector of times (as POSIX) for which the variable should be fetched. If no times
are selected (NULL, the default) all available times from the netcdf file are returned}

\item{res}{Spatial resolution for depths, if no depths are explicitly specified by 'z_out'}

\item{reference}{Either "surface" or "bottom". The reference point for the depths sepified in
'z_out'}

\item{long}{Boolean, output the variable in long format? Defaults to FALSE}
}
\description{
This function fetches variables for given depths from a netcdf file created by running GOTM-FABM
}
\examples{
\dontrun{
temp <- get_var(file = "output.nc", var = "temp")
}

}
\author{
Johannes Feldbauer
}
\keyword{FABM,}
\keyword{GOTM,}
\keyword{get}
\keyword{variable}
