#' statisfactory: Species distribution modeling and ecological niche modeling
#'
#' This package contains various statistical tools and helper functions.
#'
#' Create an issue on \href{https://github.com/adamlilith/statisfactory/issues}{GitHub}.
#'
#' @details
#' @section Utilities:
#' \code{\link{makeFormulae}}: Make all possible formula
#'
#' @section Calculations:
#' \code{\link{euclid}}: Euclidean distance
#' \code{\link{geoMean}}: Geometric mean
#' \code{\link{invLogitAdj}}: Inverse logit transform robust to 0's or 1's
#' \code{\link{logitAdj}}: Logit transform robust to 0's and 1's
#' \code{\link{se}}: Standard error of the mean
#'
#' @section Data transformation:
#' \code{\link{art}}: Aligned rank transform for using ANOVAs on rank data
#' \code{\link{rankMulti}}: Rank values by multiple tie-breaking criteria
#'
#' @section Sampling:
#' \code{\link{sampleAcross}}: Permute values across two vectors or columns in two data frames or matrices
#' \code{\link{sampleStrat}}: Sample values stratified by one or more other variables
#'
#' @section Statistics:
#' \code{\link{nagelR2}}: Nagelkerge's / Craig & Uhler's R2
#'
#' @section Plots:
#' \code{\link{histOverlap}}: Histogram with overlapping or arbitrary bins
#' \code{\link{hist2d}}: Two-dimensional histogram
#'
#' @docType package
#' @author Adam B. Smith
#' @name statisfactory
#' @keywords internal
NULL
