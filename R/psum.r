#' Element-by-element sum
#'
#' This function is similar to \code{\link{pmax}} or \code{\link{pmin}}, except that it returns the element-wise sum of values. If the input is a \code{matrix} or \code{data.frame}, the output is the same as \code{\link{colSums}}.
#'
#' @param ... A set of vectors of the same length, a \code{matrix}, or a \code{data.table}.
#' @param na.rm If \code{FALSE} (default), return \code{NA} if any element in a set is \code{NA}.
#'
#' @details Adapted from answer by Ben Bolker on \href{https://stackoverflow.com/questions/13123638/there-is-pmin-and-pmax-each-taking-na-rm-why-no-psum}[Stackoverflow].
#'
#' @return A numeric vector.
#' @examples
#'
#' x1 <- 1:10
#' x2 <- runif(10)
#' psum(x1, x2)
#'
#' x <- cbind(x1, x2)
#' psum(x)
#'
#' x2[3] <- NA
#' psum(x1, x2)
#' psum(x1, x2, na.rm=TRUE)
#' 
#' @export

psum <- function(..., na.rm=FALSE) {
	
	x <- list(...)
	lengths <- sapply(x, length)
	if (!length(unique(x)) == 1L) stop('All arguments must be the same length.')

	rowSums(do.call(cbind, x), na.rm=na.rm)
	
} 	
