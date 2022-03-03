#' Count number of contiguous "blocks" of cells
#'
#' This function calculates the number of objects formed by one or more adjacent cells that touch on their edges (i.e., not just at a corner). One way to solve this (inefficiently) is using a "ink-spreading" algorithm that accumulates adjacent cells until all are accounted for, then counts this as a single component. This function uses an efficient solution based on the Euler characteristic.
#'
#' @param x Matrix
#' @param count Value to count as a "presence" in the matrix. All other values will be assumed to be not part of a component.
#'
#' @details Inspired by an answer by Alon Amit to the question on Quora, "\emph{What are some programming problems that look hard at a first glance but are actually easy?}".
#' @return An integer (the number of connected, non-conterminous components).
#' @examples
#'
#' v <- c(
#' 1, 1, 0, 1,
#' 1, 1, 0, 0,
#' 1, 0, 0, 0,
#' 0, 0, 0, 1,
#' 0, 0, 1, 1,
#' 1, 0, 0, 0,
#' 0, 0, 0, 0)
#'
#' x <- matrix(v, ncol=4, byrow=TRUE)
#' x
#'
#' countConnected(x)
#'
#' \dontrun{
#' # will break because of connection at a vertex
#' v <- c(
#' 1, 1, 0, 1,
#' 1, 1, 0, 0,
#' 1, 0, 0, 0,
#' 0, 0, 0, 1,
#' 0, 0, 1, 1,
#' 1, 0, 0, 0,
#' 0, 1, 0, 0)
#'
#' x <- matrix(v, ncol=4, byrow=TRUE)
#' x
#'
#' countConnected(x)
#' }
#'
#' @export

countConnected <- function(x, count = 1) {

	# binarize
	x <- as.matrix(x)
	x[] <- ifelse(omnibus::naCompare('==', x, count), 1L, 0L)

	# pad with one cell
	add <- ncol(x)
	add <- matrix(0L, nrow=1L, ncol=add)
	x <- rbind(add, x, add)

	add <- nrow(x)
	add <- matrix(0L, nrow=add, ncol=1L)
	x <- cbind(add, x, add)

	# move in 2x2 window
	# pts = number of cases where top left cell is occupied and all others are not
	# tees = number of cases where only top left cell is unoccupied
	# components = A - B

	nrows <- nrow(x) - 1L
	ncols <- ncol(x) - 1L

	tl1 <- bl0 <- 0
	for (row in 1L:nrows) {
		for (col in 1L:ncols) {

			focal <- x[row:(row + 1L), col:(col + 1L)]
			focalSum <- sum(focal)
			if (focal[1, 1] == 1 && focal[2, 2] == 1 && focalSum == 2) stop('Components connected at just corners cannot be counted.')
			if (focal[2, 1] == 1 && focal[1, 2] == 1 && focalSum == 2) stop('Components connected at just corners cannot be counted.')

			if (focal[1, 1] == 1 & focalSum == 1) tl1 <- tl1 + 1
			if (focal[2, 2] == 0 & focalSum == 3) bl0 <- bl0 + 1

		}
	}

	comps <- tl1 - bl0
	comps

}
