# statisfactory

This package contains various statistical tools and helper functions. You can install this package in R using these commands:

`install.packages('devtools') # if you haven't done this already`  
`library(devtools)`  
`install_github('adamlilith/omnibus')`  
`install_github('adamlilith/statisfactory')`  

## Functions ##
* `art`: Aligned rank transform for using ANOVAs on rank data
* `geoMean`: Geometric mean
* `hist2d`: Two-dimensional histogram
* `histOverlap`: Histogram with overlapping or arbitrary bins
* `logitAdj`: Logit transform robust to 0's and 1's
* `makeFormulae`: Make all possible formula
* `mmode`: Modal value
* `probitAdj`: Probit transform robust to 0's or 1's
* `rankMulti`: Rank values by multiple tie-breaking criteria
* `sampleAcross`: Permute values across two vectors or columns in two data frames or matrices
* `sampleStrat`: Sample values stratified by one or more other variables
* `se`: Standard error of the mean

Adam B. Smith