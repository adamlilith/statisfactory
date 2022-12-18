#' Calculate the mode of numeric, character, or factor data
#'
#' @param x Numeric, character, or factor vector.
#' @param na.rm Logical. If \code{TRUE} then remove \code{NA}s first. Otherwise fail.
#' @return Numeric, character, or factor value.
#' @examples
#'
#' mmode(round(10 * rnorm(1000, 2)))
#' mmode(c('a', 'b', 'b', 'b', 'c', 'd', 'd'))
#'
#' @export

mmode <- compiler::cmpfun(function(
	x,
	na.rm=FALSE
) {

	### NOTE TO SELF: ALSO APPEARS IN omnibus
	
	if (na.rm) x <- x[!is.na(x)]
	ux <- unique(x)
	m <- ux[which.max(tabulate(match(x, ux)))]
	m
	
})
