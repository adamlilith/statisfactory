#' statisfactory: Statistical and Geometrical Tools
#'
#' This package contains various statistical tools and helper functions.
#'
#' Create an issue on \href{https://github.com/adamlilith/statisfactory/issues}{GitHub}.
#'
#' @details
#' @section Utilities:
#' \code{\link{makeFormulae}}: Make all possible formula \cr
#'
#' @section Calculations:
#' \code{\link{geoMean}}: Geometric mean \cr
#' \code{\link{invLogitAdj}}: Inverse logit transform robust to 0's or 1's \cr
#' \code{\link{logitAdj}}: Logit transform robust to 0's and 1's \cr
#' \code{\link{psum}}: Element-wise sum (like pmin and pmax) \cr
#' \code{\link{se}}: Standard error of the mean \cr
#'
#' @section Distance measures:
#' \code{\link{euclid}}: Euclidean distance \cr
#' \code{\link{fuzzyJaccard}}: Fuzzy Jaccard index \cr
#'
#' @section Data transformation:
#' \code{\link{art}}: Aligned rank transform for using ANOVAs on rank data \cr
#' \code{\link{rankMulti}}: Rank values by multiple tie-breaking criteria \cr
#'
#' @section Principal component analysis (PCA):
#' \code{\link{backTransPCA}}: "Back-transform" PCA scores to their original values \cr
#'
#' @section Geometry:
#' \code{\link{countConnected}}: Count number of contiguous "blocks" of cells \cr
#'
#' @section Sampling:
#' \code{\link{sampleAcross}}: Permute values across two vectors or columns in two data frames or matrices \cr
#' \code{\link{sampleStrat}}: Sample values stratified by one or more other variables \cr
#'
#' @section Statistics:
#' \code{\link{nagelR2}}: Nagelkerge's / Craig & Uhler's R2 \cr
#'
#' @section Histograms:
#' \code{\link{histOverlap}}: Histogram with overlapping or arbitrary bins \cr
#' \code{\link{hist2d}}: Two-dimensional histogram \cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name statisfactory
#' @keywords internal
NULL
