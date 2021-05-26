# statisfactory

This package contains various statistical tools and helper functions. You can install this package in R using these commands:

`remotes::install_github('adamlilith/omnibus', dependencies=TRUE)`  
`remotes::install_github('adamlilith/statisfactory', dependencies=TRUE)`  

NB: If for some reason these commands do not work, you can install the package(s) by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package(s) manually.

### Functions ###
## Utilities ##
* `makeFormulae`: Make all possible formula

## Calculations ##
* `euclid`: Euclidean distance
* `geoMean`: Geometric mean
* `invLogitAdj`: Probit transform robust to 0's or 1's
* `logitAdj`: Logit transform robust to 0's and 1's
* `mmode`: Modal value
* `se`: Standard error of the mean

## Data transformation ##
* `art`: Aligned rank transform for using ANOVAs on rank data
* `rankMulti`: Rank values by multiple tie-breaking criteria

## Sampling ##
* `sampleAcross`: Permute values across two vectors or columns in two data frames or matrices
* `sampleStrat`: Sample values stratified by one or more other variables

## Statistics ##
* `nagelR2`: Nagelkerge's / Craig & Uhler's R2

## Plots ##
* `hist2d`: Two-dimensional histogram
* `histOverlap`: Histogram with overlapping or arbitrary bins


Adam B. Smith