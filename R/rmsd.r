#' Root-mean-square deviation
#'
#' Calculate the root-mean-square deviation (\code{sqrt(mean((x1 - x2)^2))}).
#' @param x1 Numeric vector.
#' @param x2 Numeric vector same length as \code{x1}.
#' @param na.rm Logical, if \code{TRUE} then remove any elements in \code{x1} \emph{and} \code{x2} where either \code{x1} or \code{x2} is \code{NA}. Default is \code{FALSE}, in which case any \code{NA} returns \code{NA}.
#' @return Numeric.
#' @examples
#' set.seed(123)
#' x1 <- 1:20
#' x2 <- 1:20 + rnorm(20)
#' rmsd(x1, x2)
#' x1[1] <- NA
#' rmsd(x1, x2)
#' rmsd(x1, x2, na.rm=TRUE)
#' @export

rmsd <- compiler::cmpfun(function(
	x1,
	x2,
	na.rm = FALSE
) {

	if (length(x1) != length(x2)) {
		stop('Arguments "x1" and "x2" must have same length in function "rmsd".')
	}

	if (na.rm) {
		cleaned <- naOmitMulti(x1, x2)
		x1 <- cleaned[[1]]
		x2 <- cleaned[[2]]
	}
	
	out <- if (length(x1) > 0 & length(x2) > 0) {
		sqrt(mean((x1 - x2)^2))
	} else {
		NA
	}
	
	out

})
