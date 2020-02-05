# rodeoFABM
tool for creating fortran code for a FABM biogeochemical model from data.frames. Similar to the [rodeo](https://github.com/dkneis/rodeo) R-package it uses the well known standard notation based on the [stoichiometry matrix](https://en.wikipedia.org/wiki/Petersen_matrix) to describe the ODEs.

## installation
you can install `rodeoFABM` from github using:

```r
install_packages("devtools")
devtools::install_github("JFeldbauer/rodeoFABM")
```

## using FABMrodeo

### clone GOTM-FABM source code

You can clone and build the lake branche of GOTM-FABM using the function `clone_GOTM()`:

```r
library(rodeoFABM)
# clone github repo
clone_GOTM(build_dir = "build",src_dir = "gotm")
```
after you generated FABM code you can compile the model


### generate FABM code and compile GOTM-FABM

to generate FABM code from data.frames (e.g. storred in a Libre Office spread sheet) use:

```r
library(readODS)
library(rodeoFABM)

# copy example ods file
example_model <- system.file("extdata//", package= 'rodeoFABM')
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

# generate fabm code
gen_fabm_code(vars,pars,funs,pros,stoi,"simple_model.f90",diags = TRUE)

# build GOTM
build_GOTM(build_dir = "../build",fabm_file = "simple_model.f90",
           src_dir = "../gotm/extern/fabm/src/models/tuddhyb/rodeo")

```
