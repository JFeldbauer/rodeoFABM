% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document_model.R
\name{document_model}
\alias{document_model}
\title{Function to create LaTeX documentation of a model}
\usage{
document_model(
  vars,
  pars,
  pros,
  funs,
  stoi,
  landscape = TRUE,
  tex = "tex",
  ad_col = list(),
  name = "",
  nosplit = FALSE
)
}
\arguments{
\item{vars}{data.frame containing the state variables}

\item{pars}{data.frame containing the parameters}

\item{pros}{data.frame containing the processes}

\item{funs}{data.frame containing the functions}

\item{stoi}{data.frame containing the stoichiometry}

\item{landscape}{boolean, should the process expression table be in landscape?}

\item{tex}{name of the column containing the LaTeX expressions to use as symbols}

\item{ad_col}{named list of additional columns to add to the tables for vars, pars, 
funs, pros, and stoi. The elements of this list must be the corresponding name (e.g. vars)
and the list elements should give the name of the additional column (col_name), an
alternative name for the column in the created table (name_out), and a logical value if the
column should be created in math mode (math).}

\item{name}{additional string to paste to the created tex files}

\item{nosplit}{do not split equations automatically. Sometimes LaTeX will throw an error due to
splitting equations over several lines, this option can prevent this.}
}
\description{
This function creates LaTeX tables from the tables used to generate the FABM source code.
It will generate a table with the statevariables, a table with the model parameter, a table
with tzhe process names and descriptions, a table with the process expressions, and a table
with the stoichiometry table.
}
\examples{
\dontrun{
library(readODS)
library(rodeoFABM)

# copy example ods file
example_model <- system.file("extdata/simple_model.ods", package= 'rodeoFABM')
dir.create('example') # Create example folder
file.copy(from = example_model, to = 'example',recursive = TRUE)
setwd('example') # Change working directory to example folder

# read in example ods file
odf_file <- "simple_model.ods"
vars <- read_ods(odf_file,1)
pars <- read_ods(odf_file,2)
funs <- read_ods(odf_file,3)
pros <- read_ods(odf_file,4)
stoi <- read_ods(odf_file,5)

# generate documentation
document_model(vars,pars,pros,funs,stoi)

## example with additional columns
document_model(vars, pars, pros, funs, stoi, landscape = FALSE,
               ad_col = list(vars = list(col_name = "default",
                                         name_out = "init. value",
                                         math = FALSE),
                             pars = list(col_name = "default",
                                         name_out = "value",
                                         math = FALSE)))
}

}
\author{
Johannes Feldbauer
}
\keyword{FABM,}
\keyword{GOTM,}
\keyword{LaTeX}
\keyword{document,}
