% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_tools.R
\name{clone_GOTM}
\alias{clone_GOTM}
\title{Function to clone GOTM-FABM from github}
\usage{
clone_GOTM(build_dir = "build", src_dir = "gotm", newest_version = TRUE)
}
\arguments{
\item{build_dir}{Directory to build GOTM-FABM in.}

\item{src_dir}{Directory to save the source code in.}

\item{newest_version}{Boolean. Should the newest version of GOTM be used. Might break if there
are substantial changes in GOTM. Fallback uses the Version of November 2019.}
}
\description{
This function clones GOTM-FABM from the GOTM github page and prepares comiplation using cmake.
Requires cmake. Builds the lake branche of GOTM
}
\examples{
\dontrun{
clone_GOTM(build_dir = "build",src_dir = "gotm")
}
}
\author{
Johannes Feldbauer
}
\keyword{FABM,}
\keyword{GOTM,}
\keyword{compile}
