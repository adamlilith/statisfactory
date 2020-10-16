#' Euclidean distance
#'
#' Euclidian distance in any dimension
#' @param a Numeric vector.
#' @param b Numeric vector of same length as \code{a}.
#' @return Numeric.
#' @examples
#' euclid(0, 5)
#' euclid(c(0, 0), c(1, 1))
#' euclid(c(0, 0, 0), c(1, 1, 1))
#' @export

euclid <- compiler::cmpfun(function(a, b) {

	if (length(a) != length(b)) stop('Length of "a" must be same as length of "b".')
	sqrt(sum((a - b)^2))
	
})
