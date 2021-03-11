# rodeoFABM
tool for creating fortran code for a FABM biogeochemical model from data.frames. Similar to the [rodeo](https://github.com/dkneis/rodeo) R-package it uses the well known standard notation based on the [stoichiometry matrix](https://en.wikipedia.org/wiki/Petersen_matrix) to describe the ODEs.

## installation
you can install `rodeoFABM` from github using:

```r
library("devtools")
install_github("JFeldbauer/rodeoFABM")
```

## using FABMrodeo

### clone GOTM-FABM source code

You can clone the lake branche of GOTM-FABM from github using the function `clone_GOTM()`:

```r
library(rodeoFABM)
# clone github repo
clone_GOTM(build_dir = "build", src_dir = "gotm")
```
after you generated FABM code you can compile the model


### generate FABM code and compile GOTM-FABM

to generate FABM code from data.frames (e.g. storred in a Libre Office spread sheet) use:

```r
library(readODS)

# copy example ods file
example_model <- system.file("extdata/simple_model.ods", package = 'rodeoFABM')
# Create example folder
dir.create('example') 
file.copy(from = example_model, to = 'example', recursive = TRUE)
# Change working directory to example folder
setwd('example') 

# read in example ods file
odf_file <- "simple_model.ods"
vars <- read_ods(odf_file, 1)
pars <- read_ods(odf_file, 2)
funs <- read_ods(odf_file, 3)
pros <- read_ods(odf_file, 4)
stoi <- read_ods(odf_file, 5)

# generate fabm code
gen_fabm_code(vars, pars, funs, pros, stoi, "simple_model.f90", diags = TRUE)

# build GOTM
build_GOTM(build_dir = "../build", fabm_file = "simple_model.f90",
           src_dir = "../gotm")

```

### run GOTM-FABM

```r
library(gotmtools)
library(OceanView)

# copy gotm control file and initital temperature
yaml_file <- system.file("extdata/gotm.yaml", package= "rodeoFABM")
file.copy(from = yaml_file, to = ".", recursive = TRUE)

# load forcing data
data(meteo_file)
data(hypsograph)
write.table(meteo_file, "meteo_file.dat", sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = TRUE)
write.table(hypsograph, "hypsograph.dat", sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = TRUE)

# run GOTM-FABM
system2("./gotm")

# read in model output
ALG1 <- get_vari("output.nc", "rodeo_C_ALG")
death <- get_vari("output.nc", "rodeo_death")
growth <- get_vari("output.nc", "rodeo_growth")

# plot model output
image2D(t(apply(as.matrix(ALG1[ , -1]), 1, rev)), ALG1$Datetime,
        seq(-47, 0, length.out = 100), main = "ALG", xlab = "Date", ylab = "Depth")

image2D(t(apply(as.matrix(growth[ , -1]), 1, rev)) - 
          t(apply(as.matrix(death[ ,  -1]), 1, rev)), growth$Datetime,
        seq(-47, 0, length.out = 100), main = "net. growth",
        xlab = "Date", ylab = "Depth",
        col = ramp.col(c("red4", "grey", "green4")))
```
### Funding
Work on this project was funded by the European Social Fund and co-financed by tax funds based on the budget
approved by the members of the Saxon State Parliament.

