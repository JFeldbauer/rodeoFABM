# rodeoFABM
tool for creating fortran code for a FABM biogeochemical model from data.frames. Similar to the [rodeo](https://github.com/dkneis/rodeo) R-package it uses the well known standard notation based on the [stoichiometry matrix](https://en.wikipedia.org/wiki/Petersen_matrix) to describe the ODEs.

## installation
you can install `rodeoFABM` from github using:

```R
install_packages("devtools")
devtools::install_github("JFeldbauer/rodeoFABM")
```

## using FABMrodeo

### generate FABM code

to generate FABM code from data.frames (e.g. storred in a Libre Office spread sheet) use:

```R
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

```
### Compile the physical host model and FABM

at first you need to create a new institut and make FABM aware of your model: see [FABM github wiki](https://github.com/fabm-model/fabm/wiki/Developing-a-new-biogeochemical-model#create-an-institute-directory-for-your-model)

then you need move the file into the institut directory of FABM and compile your physical host model together with FABM

Compilation instructions for GOTM under Linux/Mac can be found [here](https://gotm.net/software/linux/) and for Windows (here)[https://gotm.net/software/windows/]
