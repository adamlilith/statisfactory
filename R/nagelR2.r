#' Nagelkerge's / Craig & Uhler's R2
#'
#' Nagelkerge's / Craig & Uhler's R2
#' @param likeNull Likelihood (not log-likelihood) of the null model or an object of class \code{logLik} with log-likelihood of the null model (usually an intercept-only model).
#' @param likeFull Likelihood (not log-likelihood) of the "full" model or an object of class \code{logLik} with log-likelihood of the "full" model (usually a model with covariates).
#' @param n Sample size.
#' @return Numeric.
#' @examples
#' 
#' # create data
#' x <- 1:100
#' y <- 2 + 1.7 * x + rnorm(100, 0, 30)
#' 
#' # models
#' nullModel <- lm(y ~ 1)
#' fullModel <- lm(y ~ x)
#' 
#' # plot
#' plot(x, y)
#' abline(nullModel, col='red')
#' abline(fullModel, col='blue')
#' legend('bottomright', legend=c('Null', 'Full'), lwd=1, col=c('red', 'blue'))
#' 
#' # R2
#' likeNull <- exp(as.numeric(logLik(nullModel)))
#' likeFull <- exp(as.numeric(logLik(fullModel)))
#' nagelR2(likeNull, likeFull, 100)
#' @export

nagelR2 <- compiler::cmpfun(function(likeNull, likeFull, n) {

	if ('logLik' %in% class(likeNull)) likeNull <- exp(as.numeric(likeNull))
	if ('logLik' %in% class(likeFull)) likeFull <- exp(as.numeric(likeFull))
	
	(1 - (likeNull / likeFull)^(2 / n)) / (1 - likeNull)^(2 / n)
	
})
