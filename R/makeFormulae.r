#' Make all possible formula
#'
#' This functions creates a list of formulae that contain all possible linear, quadratic, and two-way interaction terms from predictors in an object of class \code{formula}. The formulae respect marginality conditions (e.g. they will always include linear terms of variables also included as quadratic or interaction terms). Note that if there are more than several terms (i.e., >=3) and interactions and/or quadratic terms are desitred, then formula generation may take a long time.
#' @param formula A \code{formula} object with \emph{just} linear terms.
#' @param intercept Logical, if \code{TRUE} (default) then all models include an intercept.  If \code{FALSE} then then formula will specify that regression occurs through the origin (e.g., \code{y ~ -1 + etc.})
#' @param interceptOnly Logical, if \code{TRUE} then an intercept-only model is included in final set.
#' @param linearOnly Logical, if TRUE then models with only linear terms are included in final set (plus other kinds of models if desired).
#' @param quad Logical, if TRUE then include quadratic terms.
#' @param verboten List of lists, used to specify specific combinations of terms that should not occur together. See section \emph{Details} below. Ognored if \code{NULL} (default).
#' @param minTerms 
#' @param maxTerms 
#' @param returnFx 
#' @param verbose 
#' @return
#' @details  The argument \code{verboten} can be used to specify variables or terms that should not occur in the same formula. The argument \code{verboten} is composed of a list of lists. Each sublist comprises names of two variables or terms stated as characters followed by two logical values (\code{TRUE}/\code{FALSE}). The second variable/term is removed from the model if the first is in the model. If the first logical value is \code{TRUE} then the second variable/term is removed if the first variable appears alone in the formula (e.g., not in an interaction with another variable). If the first logical value is \code{FALSE} then the second variable/term is removed if the first variable/term appears in any term (e.g., as an interaction with another term). 
#' Examples: \itemize{
#' \item \code{verboten=list(list('x1', 'x2', TRUE, TRUE))}: Removes \code{x2} if \code{x1} occurs in the model as a linear term.
#' \item \code{verboten=list(list('x1', 'x2', FALSE, TRUE))}: Removes the linear term \code{x2} if \code{x1} occurred in \emph{any} term in the model.
#' \item \code{verboten=list(list('x1', 'x2', TRUE, FALSE))}: Removes \emph{any} term with \code{x2} if the linear term \code{x1} occurred in the model.
#' \item \code{verboten=list(list('x1', 'x2', FALSE, FALSE))}: Removes any term with \code{x2} if any term had \code{x1}.
#' }
#' Quadratic terms and interaction terms can also be stated, so: \itemize{
#' \item \code{verboten=list(list('x1', 'x1:x2', TRUE, TRUE))}: Removes \code{x1:x2} if \code{x1} were in the model.
#' \item \code{verboten=list(list('x1', 'I(x2^2)', TRUE, TRUE))}: Removes \code{I(x2^2)}.
#' }
#' Note that inexact matching can remove terms incorrectly if inexact matches exist between names of terms or variables.  For example, if using an inexact match, then \code{verboten(list('x1', 'x2', FALSE, FALSE))} will find any term that has an \code{x1} (e.g., \code{x11}) and if it exists, remove any term with an \code{x2} (e.g., \code{x25}). Note that reciprocally removing predictors makes little sense since, for example \code{list(list('x1', 'x2', FALSE, FALSE), list('x2', 'x1', FALSE, FALSE))} removes all formulae with \code{x2} if \code{x1} appears then tries to find any models with \code{x2} that have \code{x1} (of which there are none).
#' @examples
#' makeFormulae(y ~ x1 + x2 + x3, maxTerms=3)
#' makeFormulae(y ~ x1 + x2 + x3, ia=FALSE, maxTerms=3)
#' verboten <- list(list('x1', 'x2', TRUE, TRUE))
#' makeFormulae(y ~ x1 + x2 + x3, verboten=verboten, maxTerms=3)
#' @export
makeFormulae <- function(
	formula,
	intercept=TRUE,
	interceptOnly=TRUE,
	linearOnly=TRUE,
	quad=TRUE,
	ia=TRUE,
	verboten=NULL,
	minTerms=NULL,
	maxTerms=NULL,
	returnFx=as.formula,
	verbose=FALSE
) {

# removes redundant models from a list
removeRedundantModels <- function(m) {

	# m		a list object with each element being a character list of terms

	m <- lapply(m, FUN=function(x) sort(unique(x)))
	numTerms <- unlist(lapply(m, length))
	
	keep <- list()
	
	for (thisTerms in unique(numTerms)) {
	
		mTheseTerms <- m[which(numTerms==thisTerms)]
		mTheseTerms <- if (thisTerms==1) { matrix(sapply(mTheseTerms, paste), nrow=1) } else { sapply(mTheseTerms, paste) }
		
		newTerms <- rep('eq', ncol(mTheseTerms))
		
		for (countRow in 1:nrow(mTheseTerms)) newTerms <- paste(newTerms, mTheseTerms[countRow, ])
		
		uniqueTerms <- unique(mTheseTerms, MARGIN=2)
		
		for (countCol in 1:ncol(uniqueTerms)) keep[[length(keep) + 1]] <- c(uniqueTerms[ , countCol])
		
	}
	
	keep
	
}

####################
## PRE-PROCESSING ##
####################

pred <- sort(attr(terms(formula), 'term.labels')) # character list of predictors

stopAt <- if (is.null(maxTerms)) { length(pred) } else { min(length(pred), maxTerms) } # stop making candidate models when this number of terms have been reached

##########
## MAIN ##
##########

### calculate all possible models with just linear terms
if (verbose) cat('Making formulae with just linear terms...\n'); flush.console()

linearModels <- list()

for (countTerms in 1:stopAt) {

	modelAsList <- apply(combn(pred, countTerms), 2, as.list)
	for (count in 1:length(modelAsList)) linearModels[[length(linearModels) + 1]] <- unlist(modelAsList[[count]])
	
}

if (!is.null(verboten)) linearModels <- removeVerboten(m=linearModels, verboten=verboten)
models <- if (linearOnly) { linearModels } else { list() }

### calculate all possible quadratic models
if (quad) {

	quadModels <- list()

	# all quadratic models with just linear terms that appear also as quadratic terms
	if (verbose) cat('Making formulae with just linear terms that appear also as quadratic\n   terms...\n'); flush.console()
	
	for (countTerms in 1:stopAt) {

		modelAsList <- apply(combn(pred, countTerms), 2, as.list)
		for (count in 1:length(modelAsList)) quadModels[[length(quadModels) + 1]] <- 
			c(unlist(modelAsList[[count]]), paste('I(', unlist(modelAsList[[count]]), '^2)', sep=''))
		
	}

	# remove models above a given order
	if (!is.null(maxTerms)) {

		numTerms <- unlist(lapply(quadModels, length))
		wantModels <- which(numTerms <= maxTerms)
		
		selectModels <- list()
		for (want in wantModels) selectModels[[length(selectModels) + 1]] <- quadModels[[want]]
		quadModels <- selectModels

	}

	# remove models with forbidden term combinations
	if (!is.null(verboten)) quadModels <- removeVerboten(m=quadModels, verboten=verboten)
	
	# add models with linear terms that do not also appear as quadratic terms
	if (verbose) cat('Making formulae with quadratic and appropriate linear terms plus linear terms\n   that do not also appear as quadratic terms...\n'); flush.console()
	
	for (countQuad in 1:length(quadModels)) {
	
		for (countLinear in 1:length(linearModels)) {
		
			quadModels[[length(quadModels) + 1]] <- unique(c(linearModels[[countLinear]], quadModels[[countQuad]]))
		
		}
		
	}

	# remove redundant models
	quadModels <- removeRedundantModels(quadModels)

	# remove models above a given order
	if (!is.null(maxTerms)) {

		numTerms <- unlist(lapply(quadModels, length))
		wantModels <- which(numTerms <= maxTerms)
		
		selectModels <- list()
		for (want in wantModels) selectModels[[length(selectModels) + 1]] <- quadModels[[want]]
		quadModels <- selectModels

	}

	# remove models with forbidden term combinations
	if (!is.null(verboten)) quadModels <- removeVerboten(m=quadModels, verboten=verboten)

	# remember
	for (count in 1:length(quadModels)) models[[length(models) + 1]] <- quadModels[[count]]
	
}

### calculate all possible models with 2-ways interactions
if (ia & length(pred) > 1) {

	iaModels <- list()

	# all models with 2-way interactions where linear terms also appear in interaction terms
	if (verbose) cat('Making formulae with two-way interaction and appropriate linear terms...\n'); flush.console()
	
	for (countTerms in 2:stopAt) {
	
		theseTerms <- combn(pred, countTerms)
	
		linearRows <- nrow(theseTerms)
	
		for (countTermOne in 1:(linearRows - 1)) {
		
			for (countTermTwo in (countTermOne + 1):linearRows) {
		
				theseTerms <- rbind(theseTerms, paste(theseTerms[countTermOne, ], theseTerms[countTermTwo, ], sep=':'))
		
			}
		
		}
		
		for (countModels in 1:ncol(theseTerms)) iaModels[[length(iaModels) + 1]] <- c(theseTerms[ , countModels])
		
	}
	
	# remove models above a given order
	if (!is.null(maxTerms)) {

		numTerms <- unlist(lapply(iaModels, length))
		wantModels <- which(numTerms <= maxTerms)
		
		selectModels <- list()
		for (want in wantModels) selectModels[[length(selectModels) + 1]] <- iaModels[[want]]
		iaModels <- selectModels

	}

	# if there are any IA models
	if (length(iaModels) > 0) {
	
		# remove models with forbidden term combinations
		if (!is.null(verboten)) iaModels <- removeVerboten(m=iaModels, verboten=verboten)
		
		# all models with 2-way interaction terms with appropriate linear terms and some other linear terms that are not part of interaction terms
		if (verbose) cat('Making formulae with two-way interaction and appropriate linear terms plus\n   other linear terms that are not part of interaction terms...\n'); flush.console()

		for (countIa in 1:length(iaModels)) {
		
			for (countLinear in 1:length(linearModels)) {
			
				iaModels[[length(iaModels) + 1]] <- unique(c(linearModels[[countLinear]], iaModels[[countIa]]))
			
			}
			
		}

		# remove redundant models
		iaModels <- removeRedundantModels(iaModels)

		# remove models above a given order
		if (!is.null(maxTerms)) {

			numTerms <- unlist(lapply(iaModels, length))
			wantModels <- which(numTerms <= maxTerms)
			
			selectModels <- list()
			for (want in wantModels) selectModels[[length(selectModels) + 1]] <- iaModels[[want]]
			iaModels <- selectModels

		}

		# remove models with forbidden term combinations
		if (!is.null(verboten)) iaModels <- removeVerboten(m=iaModels, verboten=verboten)

		# remember
		for (count in 1:length(iaModels)) models[[length(models) + 1]] <- iaModels[[count]]
		
	}
	
}	

### all possible models with quadratic and interaction terms	
if (quad & ia & length(iaModels) > 0) {

	iaQuadModels <- list()

	# add quadratic and interaction models
	if (verbose) cat('Making formulae with all possible two-way interaction, quadratic, and\n   appropriate linear terms...\n'); flush.console()
	for (countQuad in 1:length(quadModels)) {

		for (countIa in 1:length(iaModels)) {
		
			iaQuadModels[[length(iaQuadModels) + 1]] <- c(quadModels[[countQuad]], iaModels[[countIa]])
			
		}
		
	}

	# remove redundant models
	iaQuadModels <- removeRedundantModels(iaQuadModels)

	# remove models above a given order
	if (!is.null(maxTerms)) {

		numTerms <- unlist(lapply(iaQuadModels, length))
		wantModels <- which(numTerms <= maxTerms)
		
		selectModels <- list()
		for (want in wantModels) selectModels[[length(selectModels) + 1]] <- iaQuadModels[[want]]
		iaQuadModels <- selectModels

	}

	# remove models with forbidden term combinations
	if (!is.null(verboten)) iaQuadModels <- removeVerboten(m=iaQuadModels, verboten=verboten)

	# add linear terms that may not be in combined quad and interaction models
	if (verbose) cat('Making formulae with all possible two-way interaction, quadratic, and\n   appropriate linear terms plus linear terms not already in formula...\n'); flush.console()

	# if there are still any IA quadratic models
	if (length(iaQuadModels) > 0) {
		
		for (countIaQuad in 1:length(iaQuadModels)) {

			for (countLinear in 1:length(linearModels)) {
			
				iaQuadModels[[length(iaQuadModels) + 1]] <- c(linearModels[[countLinear]], iaQuadModels[[countIaQuad]])
		
			}
		
		}

		# remove redundant models
		iaQuadModels <- removeRedundantModels(iaQuadModels)

		# remove models with forbidden term combinations
		if (!is.null(verboten)) iaQuadModels <- removeVerboten(m=iaQuadModels, verboten=verboten)

		# remember
		for (count in 1:length(iaQuadModels)) models[[length(models) + 1]] <- iaQuadModels[[count]]
		
	}

}

### remove models below a desired order
if (!is.null(minTerms)) {

	numTerms <- unlist(lapply(models, length))
	wantModels <- which(numTerms >= minTerms)
	
	selectModels <- list()
	for (want in wantModels) selectModels[[length(selectModels) + 1]] <- models[[want]]
	models <- selectModels

}

### remove models above a desired order
if (length(models) > 0 & !is.null(maxTerms)) {

	numTerms <- unlist(lapply(models, length))
	wantModels <- which(numTerms <= maxTerms)
	
	selectModels <- list()
	for (want in wantModels) selectModels[[length(selectModels) + 1]] <- models[[want]]
	models <- selectModels

}

### remove redundants
if (length(models) > 0) models <- removeRedundantModels(models)

### make model formulae
if (length(models) > 0) {

	form <- list()
	for (countModels in 1:length(models)) form[[countModels]] <- paste(as.character(formula)[2], '~', ifelse(intercept, '1 +', '-1 + '), paste(models[[countModels]], collapse=' + '))

	if (interceptOnly) form[[length(form) + 1]] <- paste(as.character(formula)[2], '~', 1)

	form <- sapply(form, returnFx)
	
} else { returnFx(character()) }

#####################
## POST-PROCESSING ##
#####################

return(form)

}
