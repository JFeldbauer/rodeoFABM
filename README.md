# rodeoFABM
tool for creating fortran code for a FABM biogeochemical model from data.frames. Similar to the [rodeo](https://github.com/dkneis/rodeo) R-package it uses the well known standard notation based on the [stoichiometry matrix](https://en.wikipedia.org/wiki/Petersen_matrix) to describe the ODEs.

## installation
you can install `rodeoFABM` from github using:

```r
install_packages("devtools")
devtools::install_github("JFeldbauer/rodeoFABM")
```

## using FABMrodeo

### generate FABM code

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

```
### Compile the physical host model and FABM

at first you need to create a new institut and make FABM aware of your model: see [FABM github wiki](https://github.com/fabm-model/fabm/wiki/Developing-a-new-biogeochemical-model#create-an-institute-directory-for-your-model)

then you need move the file into the institut directory of FABM and compile your physical host model together with FABM

#### GOTM compilation in Linux

clone github repo
```sh
git clone --recursive https://github.com/gotm-model/code.git gotm
```
go into the source dirwctory
```sh
cd gotm
```
switch to lake branch
```sh
git checkout origin/lake
```
fetch submodules
```sh
git submodule update --init --recursive
```
create and switch into build folder
```sh
cd .. && mkdir build && cd build
```
build make files using cmake with correct flags for FABM and STIM
```sh
cmake ../gotm -DGOTM_USE_FABM=on -DGOTM_USE_STIM=on
```
build executable
```sh
make
```
 once you have compiled GOTM and want to update your FABM model you just need to store your model.f90 (or whatever you call it) text file in `gotm/extern/fabm/src/models/your_institut_folder` and run `make` in the build folder again.


#### GOTM compilation in Windows

Compilation instructions for GOTM for Windows are available [here](https://gotm.net/software/windows/). You also need to switch to the lake branch
