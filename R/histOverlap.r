#' Count number of values in overlapping bins
#'
#' @param x Numeric values.
#' @param breaks One integer, three numeric values, or a matrix or data frame with at least two columns:
#' \itemize{
#' 		\item Single integer: The number of overlapping bins into which to enumerate values of \code{x}. The range of \code{x} covered by the bins bins will extend from the least value minus 2.5 percent of the range to the largest value plus 2.5 percent of the range.
#' 		\item Three numeric values: The first two values are the range of covered by the bins (least and greatest). The third value is the number of bins.
#' 		\item Matrix or data frame with at least two columns. Each row corresponds to a different bin.  The first column represents the minimum values of each bin and the second column the maximum value. Subsequent columns are ignored. Note that by using this option arbitrary bins can be used--they need not overlap or even be continuous in coverage.
#' }
#' @param right Logical, if \code{TRUE} (default), then use left-open and right-closed intervals.
#' @param graph Logical, if \code{TRUE} (default), then plot frequencies.
#' @param indices Logical, if \code{TRUE}, then the output will have an attribute which is a list item with one element per bin in the output, with the indices of \code{x} that fall in each bin. Default is \code{FALSE}.
#' @return Matrix
#' @seealso \code{\link[graphics]{hist}}
#' @examples
#'
#' set.seed(123)
#' x <- rnorm(1000)
#' histOverlap(x, breaks=10, graph=TRUE)
#' histOverlap(x, breaks=c(0, 1, 10), graph=TRUE)
#' mat <- matrix(c(seq(0, 1, by=0.1), seq(0.3, 1.3, by=0.1)), ncol=2)
#' histOverlap(x, breaks=mat, graph=TRUE)
#' histOverlap(x, breaks=mat, indices=TRUE)
#'
#' @export
histOverlap <- compiler::cmpfun(function(
	x,
	breaks,
	right = TRUE,
	graph = TRUE,
	indices = FALSE
) {

	# process bin breaks
	if (inherits(breaks, 'data.frame')) {
		
		breaks <- as.matrix(breaks[ , 1:2, drop=FALSE])
		
	} else if (inherits(breaks, 'matrix')) {
	
		breaks <- breaks[ , 1:2, drop=FALSE]
		
	} else {
	
		# calculate breaks based on number of bins
		if (length(breaks) == 1) {
		
			smallest <- min(x, na.rm=TRUE)
			largest <- max(x, na.rm=TRUE)
			bins <- breaks
			
			xRange <- diff(range(x, na.rm=TRUE))
			
			smallest <- smallest - 0.025 * xRange
			largest <- largest + 0.025 * xRange
			
		# calculate breaks based on range plus number of bins
		} else if (length(breaks) == 3) {
		
			smallest <- breaks[1]
			largest <- breaks[2]
			bins <- breaks[3]

		} else {
		
			stop('Argument "breaks" must be either a single integer, three values, or a data frame or matrix.')
			
		}
		
		halfBinWidth <- (largest - smallest) / (bins + 1)
		
		breaks <- matrix(
			c(seq(smallest, largest - 2 * halfBinWidth, by=halfBinWidth),
			  seq(smallest + 2 * halfBinWidth, largest, by=halfBinWidth)),
			ncol=2, byrow=FALSE
		)
		
	}

	# clean up
	colnames(breaks) <- c('lower', 'upper')
	mids <- matrix(rowMeans(breaks), ncol=1)
	colnames(mids) <- 'middle'
	breaks <- cbind(breaks[ , 'lower', drop=FALSE], mids, breaks[ , 'upper', drop=FALSE])
	
	breaks <- cbind(breaks, matrix(NA, ncol=2, nrow=nrow(breaks)))
	colnames(breaks)[(ncol(breaks) - 1):ncol(breaks)] <- c('count', 'proportion')

	# total samples
	total <- sum(!is.na(x))
	
	nas <- sum(is.na(x))
	attr(breaks, 'NAs_in_x') <- nas

	# indices of cases in each bin
	if (indices) indicesInBin <- list()
	
	# number in bins
	for (i in 1:nrow(breaks)) {

		these <- if (right) {
			which(x > breaks[i, 'lower'] & x <= breaks[i, 'upper'])
		} else {
			which(x >= breaks[i, 'lower'] & x < breaks[i, 'upper'])
		}
		
		breaks[i, 'count'] <- length(x[these])
		if (indices) indicesInBin[[i]] <- these

	}
	
	breaks[ , 'proportion'] <- breaks[ , 'count'] / sum(breaks[ , 'count'])
	
	# plot
	if (graph) {
	
		xlim <- range(pretty(c(min(breaks[ , 'lower'], na.rm=TRUE), max(breaks[ , 'upper'], na.rm=TRUE))))
		ylim <- range(pretty(c(0, breaks[ , 'proportion'])))
		mids <- breaks[ , 'middle']
		
		graphics::plot(0, type='n', xaxt='n', yaxt='n', xlim=xlim, ylim=ylim, ylab='Proportion', xlab='Bin Midpoint')

		graphics::axis(1, xlim=xlim, ylim=ylim, at=mids, labels=mids, las=3)
		graphics::axis(2, xlim=xlim, ylim=ylim, at=pretty(c(0, breaks[ , 'proportion'])))

		cols <- rep(c('red', 'green', 'orange', 'blue', 'yellow', 'gray'), length.out=nrow(breaks))
		
		for (i in 1:nrow(breaks)) {
			
			graphics::polygon(c(breaks[i, 'lower'], breaks[i, 'upper'], breaks[i, 'upper'], breaks[i, 'lower']), c(0, 0, breaks[i, 'proportion'], breaks[i, 'proportion']), border=cols[i])
			
		}
		
	}
	
	if (indices) attr(breaks, 'indices') <- indicesInBin
	breaks

})
