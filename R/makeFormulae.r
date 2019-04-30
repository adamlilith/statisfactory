#' Make all possible formula
#'
#' This functions creates a list of formulae that contain all possible linear, quadratic, and two-way interaction terms from predictors in an object of class \code{formula}. The formulae respect marginality conditions (e.g. they will always include linear terms of variables also included as quadratic or interaction terms). Note that if there are more than several terms (i.e., >=3) and interactions and/or quadratic terms are desired, then formula generation may take a long time.
#' @param formula A \code{formula} object with \emph{just} linear terms.
#' @param intercept Logical, if \code{TRUE} (default) then all models include an intercept.  If \code{FALSE} then then formula will specify that regression occurs through the origin (e.g., \code{y ~ -1 + etc.})
#' @param interceptOnly Logical, if \code{TRUE} then an intercept-only model is included in final set.
#' @param linearOnly Logical, if TRUE then models with only linear terms are included in final set (plus other kinds of models if desired).
#' @param quad Logical, if TRUE then include quadratic terms.
#' @param verboten Character list of terms that should not appear in the models. Ignored if \code{NULL} (default). Note that using this argument only makes sense if interaction or quadratic terms are specified (if you don't a particular term to appear anywhere in the model it will be faster to remove it from \code{formula}).
#' @param verbotenCombos List of lists, used to specify specific combinations of terms that should not occur together. See section \emph{Details} below. Ignored if \code{NULL} (default).
#' @param minTerms Either a positive integer representing the minimum number of terms required to be in a model, \emph{or} \code{NULL} (default) in which case the smallest model can have just one term.
#' @param maxTerms Either a positive integer representing the maximum number of terms allowed to be in a model, \emph{or} \code{NULL} (default) in which case there is no practical limit on the number of terms in a model.
#' @param verbose Logical, if \code{TRUE} then display progress. Default is \code{FALSE}.
#' @return A vector of formulae.
#' @details  The argument \code{verbotenCombos} can be used to specify variables or terms that should not occur in the same formula. The argument \code{verbotenCombos} is composed of a list of lists. Each sublist comprises names of two variables or terms stated as characters followed by two logical values (\code{TRUE}/\code{FALSE}). The second variable/term is removed from the model if the first is in the model. If the first logical value is \code{TRUE} then the second variable/term is removed if the first variable appears alone in the formula (e.g., not in an interaction with another variable). If the first logical value is \code{FALSE} then the second variable/term is removed if the first variable/term appears in any term (e.g., as an interaction with another term). 
#' Examples: \itemize{
#' \item \code{verbotenCombos=list(list('x1', 'x2', TRUE, TRUE))}: Removes \code{x2} if \code{x1} occurs in the model as a linear term.
#' \item \code{verbotenCombos=list(list('x1', 'x2', FALSE, TRUE))}: Removes the linear term \code{x2} if \code{x1} occurred in \emph{any} term in the model.
#' \item \code{verbotenCombos=list(list('x1', 'x2', TRUE, FALSE))}: Removes \emph{any} term with \code{x2} if the linear term \code{x1} occurred in the model.
#' \item \code{verbotenCombos=list(list('x1', 'x2', FALSE, FALSE))}: Removes any term with \code{x2} if any term had \code{x1}.
#' }
#' Quadratic terms and interaction terms can also be stated, so: \itemize{
#' \item \code{verbotenCombos=list(list('x1', 'x1:x2', TRUE, TRUE))}: Removes \code{x1:x2} if \code{x1} were in the model.
#' \item \code{verbotenCombos=list(list('x1', 'I(x2^2)', TRUE, TRUE))}: Removes \code{I(x2^2)}.
#' }
#' Note that inexact matching can remove terms incorrectly if inexact matches exist between names of terms or variables.  For example, if using an inexact match, then \code{verbotenCombos(list('x1', 'x2', FALSE, FALSE))} will find any term that has an \code{x1} (e.g., \code{x11}) and if it exists, remove any term with an \code{x2} (e.g., \code{x25}). Note that reciprocally removing predictors makes little sense since, for example \code{list(list('x1', 'x2', FALSE, FALSE), list('x2', 'x1', FALSE, FALSE))} removes all formulae with \code{x2} if \code{x1} appears then tries to find any models with \code{x2} that have \code{x1} (of which there are none).
#' @examples
#' makeFormulae(y ~ x1 + x2 + x3, maxTerms=3)
#' makeFormulae(y ~ x1 + x2 + x3, ia=FALSE, maxTerms=3)
#' verboten <- c('x1:x2', 'I(x1^2)')
#' makeFormulae(y ~ x1 + x2 + x3, verboten=verboten, maxTerms=3)
#' makeFormulae(y ~ x1 + x2 + x3, verbotenCombos=verbotenCombos, maxTerms=3)
#' verbotenCombos <- list(list('x1', 'x2', TRUE, TRUE))
#' makeFormulae(y ~ x1 + x2 + x3, verbotenCombos=verbotenCombos, maxTerms=3)
#' @export
makeFormulae <- function(
	formula,
	intercept=TRUE,
	interceptOnly=TRUE,
	linearOnly=TRUE,
	quad=TRUE,
	ia=TRUE,
	verboten=FALSE,
	verbotenCombos=NULL,
	minTerms=NULL,
	maxTerms=NULL,
	returnFx=as.formula,
	verbose=FALSE
) {

	# get response and predictor(s)
	resp <- all.vars(formula)[1]
	preds <- all.vars(formula)
	preds <- preds[2:length(preds)]

	# stop making candidate models when this number of terms have been reached
	stopAt <- if (is.null(maxTerms)) { length(preds) } else { min(length(preds), maxTerms) }

	### linear term-only models
	###########################
	
		if (verbose) omnibus::say('Making formulae with just linear terms...')

		linearModels <- list()

		for (countTerms in 1:stopAt) {

			modelAsList <- apply(combn(preds, countTerms), 2, as.list)
			for (count in seq_along(modelAsList)) linearModels[[length(linearModels) + 1]] <- unlist(modelAsList[[count]])
			
		}

		if (!is.null(verboten)) linearModels <- .removeVerbotenVariables(linearModels, verboten=verboten)
		if (!is.null(verbotenCombos)) linearModels <- .removeVerbotenVariableCombos(linearModels, verbotenCombos=verbotenCombos)
		models <- if (linearOnly) { linearModels } else { list() }

	### quadratic models
	####################
		
		if (quad) {

			quadModels <- list()

			### all quadratic models with just linear terms that appear also as quadratic terms
			###################################################################################
			
				if (verbose) omnibus::say('Making formulae with just linear terms that appear also as quadratic terms...')
				
				for (countTerms in 1:stopAt) {

					modelAsList <- apply(combn(preds, countTerms), 2, as.list)
					for (count in seq_along(modelAsList)) quadModels[[length(quadModels) + 1]] <- 
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
				if (!is.null(verboten)) quadModels <- .removeVerbotenVariables(quadModels, verboten=verboten)
				if (!is.null(verbotenCombos)) quadModels <- .removeVerbotenVariableCombos(quadModels, verbotenCombos=verbotenCombos)
			
			### models with linear terms that do not also appear as quadratic terms
			#######################################################################
				
				if (verbose) cat('Making formulae with quadratic and appropriate linear terms plus linear terms that do not also appear as quadratic terms...')
				
				for (countQuad in seq_along(quadModels)) {
				
					for (countLinear in seq_along(linearModels)) {
					
						quadModels[[length(quadModels) + 1]] <- unique(c(linearModels[[countLinear]], quadModels[[countQuad]]))
					
					}
					
				}

				# remove redundant models
				quadModels <- .removeRedundantModels(quadModels)

				# remove models above a given order
				if (!is.null(maxTerms)) {

					numTerms <- unlist(lapply(quadModels, length))
					wantModels <- which(numTerms <= maxTerms)
					
					selectModels <- list()
					for (want in wantModels) selectModels[[length(selectModels) + 1]] <- quadModels[[want]]
					quadModels <- selectModels

				}

				# remove models with forbidden term combinations
				if (!is.null(verboten)) quadModels <- .removeVerbotenVariables(quadModels, verboten=verboten)
				if (!is.null(verbotenCombos)) quadModels <- .removeVerbotenVariableCombos(quadModels, verbotenCombos=verbotenCombos)

			# remember
			for (count in seq_along(quadModels)) models[[length(models) + 1]] <- quadModels[[count]]
			
		}

	### models with 2-ways interactions
	###################################
		
		iaModels <- list()

		if (ia & length(preds) > 1) {

			# all models with 2-way interactions where linear terms also appear in interaction terms
			if (verbose) omnibus::say('Making formulae with two-way interaction and appropriate linear terms...')
			
			for (countTerms in 2:stopAt) {
			
				theseTerms <- combn(preds, countTerms)
			
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
				if (!is.null(verboten)) iaModels <- .removeVerbotenVariables(iaModels, verboten=verboten)
				if (!is.null(verbotenCombos)) iaModels <- .removeVerbotenVariableCombos(iaModels, verbotenCombos=verbotenCombos)
				
				# all models with 2-way interaction terms with appropriate linear terms and some other linear terms that are not part of interaction terms
				if (verbose) cat('Making formulae with two-way interaction and appropriate linear terms plus\n   other linear terms that are not part of interaction terms...\n'); flush.console()

				for (countIa in seq_along(iaModels)) {
				
					for (countLinear in seq_along(linearModels)) {
					
						iaModels[[length(iaModels) + 1]] <- unique(c(linearModels[[countLinear]], iaModels[[countIa]]))
					
					}
					
				}

				# remove redundant models
				iaModels <- .removeRedundantModels(iaModels)

				# remove models above a given order
				if (!is.null(maxTerms)) {

					numTerms <- unlist(lapply(iaModels, length))
					wantModels <- which(numTerms <= maxTerms)
					
					selectModels <- list()
					for (want in wantModels) selectModels[[length(selectModels) + 1]] <- iaModels[[want]]
					iaModels <- selectModels

				}

				# remove models with forbidden term combinations
				if (!is.null(verboten)) iaModels <- .removeVerbotenVariables(iaModels, verboten=verboten)
				if (!is.null(verbotenCombos)) iaModels <- .removeVerbotenVariableCombos(iaModels, verbotenCombos=verbotenCombos)

				# remember
				for (count in seq_along(iaModels)) models[[length(models) + 1]] <- iaModels[[count]]
				
			}
			
		}	

	### models with quadratic and interaction terms
	###############################################

		if (quad & ia & length(iaModels) > 0) {

			iaQuadModels <- list()

			# add quadratic and interaction models
			if (verbose) omnibus::say('Making formulae with all possible two-way interaction, quadratic, and appropriate linear terms...')
			
			for (countQuad in seq_along(quadModels)) {

				for (countIa in seq_along(iaModels)) {
				
					iaQuadModels[[length(iaQuadModels) + 1]] <- c(quadModels[[countQuad]], iaModels[[countIa]])
					
				}
				
			}

			# remove redundant models
			iaQuadModels <- .removeRedundantModels(iaQuadModels)

			# remove models above a given order
			if (!is.null(maxTerms)) {

				numTerms <- unlist(lapply(iaQuadModels, length))
				wantModels <- which(numTerms <= maxTerms)
				
				selectModels <- list()
				for (want in wantModels) selectModels[[length(selectModels) + 1]] <- iaQuadModels[[want]]
				iaQuadModels <- selectModels

			}

			# remove models with forbidden term combinations
			if (!is.null(verboten)) iaQuadModels <- .removeVerbotenVariables(iaQuadModels, verboten=verboten)
			if (!is.null(verbotenCombos)) iaQuadModels <- .removeVerbotenVariableCombos(iaQuadModels, verbotenCombos=verbotenCombos)

			### add linear terms that may not be in combined quad and interaction models
			############################################################################
			
			if (verbose) omnibus::say('Making formulae with all possible two-way interaction, quadratic, and appropriate linear terms plus linear terms not already in formula...')

			# if there are still any IA quadratic models
			if (length(iaQuadModels) > 0) {
				
				for (countIaQuad in seq_along(iaQuadModels)) {

					for (countLinear in seq_along(linearModels)) {
					
						iaQuadModels[[length(iaQuadModels) + 1]] <- c(linearModels[[countLinear]], iaQuadModels[[countIaQuad]])
				
					}
				
				}

				# remove redundant models
				iaQuadModels <- .removeRedundantModels(iaQuadModels)

				# remove models with forbidden term combinations
				if (!is.null(verboten)) iaQuadModels <- .removeVerbotenVariables(iaQuadModels, verboten=verboten)
				if (!is.null(verbotenCombos)) iaQuadModels <- .removeVerbotenVariableCombos(iaQuadModels, verbotenCombos=verbotenCombos)

				# remember
				for (count in seq_along(iaQuadModels)) models[[length(models) + 1]] <- iaQuadModels[[count]]
				
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
	if (length(models) > 0) models <- .removeRedundantModels(models)

	### make model formulae
	if (length(models) > 0) {

		forms <- list()
		for (countModels in seq_along(models)) {
			forms[[countModels]] <- paste(resp, '~', ifelse(intercept, '1 +', '-1 + '), paste(models[[countModels]], collapse=' + '))
		}

		if (interceptOnly) forms[[length(forms) + 1]] <- paste(resp, '~', 1)

		forms <- sapply(forms, returnFx)
		
	} else {
		forms <- returnFx(character())
	}

	forms

}
