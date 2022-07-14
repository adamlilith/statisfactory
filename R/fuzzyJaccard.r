#' Fuzzy Jaccard index
#'
#' Calculates the fuzzy Jaccard index. The "normal" Jaccard index is given by \code{sum(A intersect B) / sum(A union B)}, where \code{A} and \code{B} are sets. Typically, \code{A} and \code{B} are binary outcomes, but the fuzzy version can accommodate values in [0, 1] and/or binary outcomes. The computationally efficient and equivalent method is \code{sum(pmin(A, B)) / (sum(A) + sum(B) - sum(pmin(A, B)))}. If \code{A} and \code{B} and both binary, the outcome is the same as the "plain" Jaccard index.
#'
#' @param a,b Vectors of binary and/or values in the range [0, 1]. The vectors must be of the same length.
#'
#' @return
#' Numeric in the range [0, 1].
#'
#' @examples
#' a <- c(0.3, 0, 0.9, 0.5)
#' b <- c(1, 1, 0, 0)
#' fuzzyJaccard(a, b)
#'
#' @export

fuzzyJaccard <- compiler::cmpfun(function(a, b) {

	la <- length(a)
	lb <- length(b)
	if (la != lb) stop('Vectors "a" and "b" must be the same length.')

	C <- sum(pmin(a, b))
	A <- sum(a)
	B <- sum(b)

	C / (A + B - C)

})
