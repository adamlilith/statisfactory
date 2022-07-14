#' "Back-transform" PCA scores to their original values
#'
#' This function back-transforms principal component scores to their original values.
#'
#' @param pca	Object of class \code{prcomp}.
#' @param x		Either \code{NULL} (default) or a vector of PC scores. If \code{NULL}, then the scores from the PCA object are used.
#'
#' @return Numeric vector.
#' @examples
#' x <- data.frame(
#' 	x1 = 1:20 + rnorm(20),
#' 	x2 = 1:20 + rnorm(20, 0, 5),
#' 	x3 = sample(20, 20)
#' )
#' 
#' pca1 <- prcomp(x, center=FALSE, scale=FALSE)
#' pca2 <- prcomp(x, center=TRUE, scale=FALSE)
#' pca3 <- prcomp(x, center=TRUE, scale=TRUE)
#' 
#' backTransPCA(pca1)
#' backTransPCA(pca2)
#' backTransPCA(pca3)
#' 
#' @export

backTransPCA <- function(pca, x = NULL) {
	
	rots <- pca$rotation
	if (is.null(x)) x <- pca$x
	x <- as.matrix(x)

	y <- as.data.frame(t(tcrossprod(rots, x)))
	
	if (!inherits(pca$scale, 'logical')) {
		scale <- rbind(pca$scale)
		scale <- scale[rep(1L, nrow(y)), ]
		y <- y * scale
	}
	
	if (!inherits(pca$center, 'logical')) {
		center <- rbind(pca$center)
		center <- center[rep(1L, nrow(y)), ]
		y <- y + center
	}
	
	y
	
}
