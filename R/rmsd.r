#' Root-mean-square deviation
#'
#' Calculate the root-mean-square deviation (\code{sqrt(mean((x1 - x2)^2))}). If non-constant weights \code{w} are supplied, then the calculation is \code{sqrt(sum(w * (x1 - x2)^2) / sum(w))}. Alternatively, \code{w} can be a function, in which case the returned value is equal to \code{sqrt(mean(w((x1 - x2)^2)))}.
#' @param x1 Numeric vector, matrix, or data frame.
#' @param x2 Numeric vector the same length as \code{x1}, or a matrix or data frame the same dimensions as \code{x1}.
#' @param w Weights or a function defining weights. If \code{x1} and \code{x2} are vectors, this can be a numeric vector the same length as \code{x1} or \code{x2}. If \code{x1} and \code{x2} are matrices or data frames then this can be either a matrix or data frame with the same dimensions as \code{x1} and \code{x2}. Alternatively, this can be a function to define weights. The function will be applied to each value of \code{(x1 - x2)^2}. The default value of \code{NULL} assigns each pair of values in \code{x1} and \code{x2} equal weight.
#' @param na.rm Logical, if \code{TRUE} then remove any elements in \code{x1} \emph{and} \code{x2} where either \code{x1} or \code{x2} is \code{NA}. Default is \code{FALSE}, in which case any \code{NA} returns \code{NA}.
#' @return Numeric.
#' @examples
#' set.seed(123)
#' # numeric vectors
#' x1 <- 1:20
#' x2 <- 1:20 + rnorm(20)
#' rmsd(x1, x2)
#' x1[1] <- NA
#' rmsd(x1, x2)
#' rmsd(x1, x2, na.rm=TRUE)
#'
#' # matrices
#' x1 <- matrix(1:20, ncol=5)
#' x2 <- matrix(1:20 + rnorm(20), ncol=5)
#' rmsd(x1, x2)
#' x1[1, 1] <- NA
#' rmsd(x1, x2)
#' rmsd(x1, x2, na.rm=TRUE)
#' 
#' # weights as values
#' x1 <- matrix(1:20, ncol=5)
#' x2 <- matrix(1:20 + rnorm(20, 0, 2), ncol=5)
#' w <- matrix(1:20, ncol=5)
#' rmsd(x1, x2)
#' rmsd(x1, x2, w)
#'
#' # weights as a function
#' x1 <- matrix(1:20, ncol=5)
#' x2 <- matrix(20:1, ncol=5)
#' w <- function(x) 1 - exp(-x)
#' rmsd(x1, x2)
#' rmsd(x1, x2, w)
#' @export

rmsd <- compiler::cmpfun(function(
	x1,
	x2,
	w = NULL,
	na.rm = FALSE
) {

	if (class(x1) == 'data.frame') x1 <- unlist(x1)
	if (class(x2) == 'data.frame') x2 <- unlist(x2)
	if (!is.null(w) && class(w) == 'data.frame') w <- unlist(w)
	
	if (class(x1) == 'matrix') x1 <- c(x1)
	if (class(x2) == 'matrix') x2 <- c(x2)
	if (!is.null(w) && class(w) == 'matrix') w <- c(w)
	
	if (length(x1) != length(x2)) {
		stop('Arguments "x1" and "x2" must have same length or dimensions.')
	}

	if (!is.null(w) && class(w) != 'function' & length(w) != length(x1)) {
		stop('Argument "w" must have the same dimensions as "x1" and "x2".')
	}

	if (is.null(w)) w <- rep(1, length(x1))

	nonNas <- if (class(w) != 'function') {
		omnibus::naOmitMulti(x1, x2, w)
	} else {
		omnibus::naOmitMulti(x1, x2)
	}
	
	x1 <- nonNas[[1]]
	x2 <- nonNas[[2]]
	if (class(w) != 'function') {
		w <- nonNas[[3]]
		totalWeight <- sum(w)
	}
	
	out <- if (length(x1) > 0 & length(x2) > 0) {
		if (class(w) != 'function') {
			sqrt(sum(w * (x1 - x2)^2) / totalWeight)
		} else {
			sqrt(mean(w((x1 - x2)^2)))
		}
	} else {
		NA
	}
	
	out

})
