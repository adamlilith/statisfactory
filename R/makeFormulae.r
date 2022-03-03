#' Make all possible formula
#'
#' This functions creates a list of formulae that contain all possible linear, quadratic, and two-way interaction terms from individual terms in an object of class \code{formula}. The formulae respect marginality conditions (i.e., they will always include lower-order terms if higher-order terms are included in a formula). Note that if there are more than several terms (i.e., >=3) and interactions and/or quadratic terms are desired, then formula generation may take a long time.
#'
#' @param formula 	A \code{formula} object with \emph{just} linear terms.
#' @param intercept Logical, if \code{TRUE} (default) then all models include an intercept.  If \code{FALSE} then then formula will specify that regression occurs through the origin (e.g., \code{y ~ -1 + etc.})
#' @param interceptOnly Logical, if \code{TRUE} then an intercept-only model is included in final set.
#' @param linearOnly Logical, if \code{TRUE} (default) then models with only linear terms are included in final set (plus other kinds of models if desired).
#' @param quad Logical, if \code{TRUE} (default), then include quadratic terms.
#' @param ia Logical, if \code{TRUE} (default), then include 2-way interaction terms.
#' @param verboten Character list of terms that should not appear in the models. Ignored if \code{NULL} (default). Note that using this argument only makes sense if interaction or quadratic terms are specified (if you don't a particular term to appear anywhere in the model it will be faster to remove it from \code{formula}).
#' @param verbotenCombos List of lists, used to specify specific combinations of terms that should not occur together. See section \emph{Details} below. Ignored if \code{NULL} (default).
#' @param minTerms Either a positive integer representing the minimum number of terms required to be in a model, \emph{or} \code{NULL} (default) in which case the smallest model can have just one term.
#' @param maxTerms Either a positive integer representing the maximum number of terms allowed to be in a model, \emph{or} \code{NULL} (default) in which case there is no practical limit on the number of terms in a model.
#' @param returnFx Function used to generate the class of the output objects. Sensible functions in include \code{\link[stats]{as.formula}} (default) or \code{\link{as.character}}.
#' @param verbose Logical, if \code{TRUE} then display progress. Default is \code{FALSE}.
#'
#' @return A vector of formulae.
#'
#' @details  The argument \code{verbotenCombos} can be used to specify variables or terms that should not occur in the same formula. The argument \code{verbotenCombos} is composed of a list of lists. Each sublist comprises names of two variables or terms stated as characters followed by two logical values (\code{TRUE}/\code{FALSE}). The second variable/term is removed from the model if the first is in the model. If the first logical value is \code{TRUE} then the second variable/term is removed if the first variable appears alone in the formula (e.g., not in an interaction with another variable). If the first logical value is \code{FALSE} then the second variable/term is removed if the first variable/term appears in any term (e.g., as an interaction with another term).
#' Examples: \itemize{
#' \item \code{verbotenCombos=list(list('x1', 'x2', TRUE, TRUE))}: Removes \code{x2} if \code{x1} occurs in the model as a linear term.
#' \item \code{verbotenCombos=list(list('x1', 'x2', FALSE, TRUE))}: Removes the linear term \code{x2} if \code{x1} occurrs in \emph{any} term in the model.
#' \item \code{verbotenCombos=list(list('x1', 'x2', TRUE, FALSE))}: Removes \emph{any} term with \code{x2} if the linear term \code{x1} occurrs in the model.
#' \item \code{verbotenCombos=list(list('x1', 'x2', FALSE, FALSE))}: Removes any term with \code{x2} if any term has \code{x1}.
#' }
#' Quadratic terms and interaction terms can also be used, so: \itemize{
#' \item \code{verbotenCombos=list(list('x1', 'x1:x2', TRUE, TRUE))}: Removes \code{x1:x2} if \code{x1} were in the model.
#' \item \code{verbotenCombos=list(list('x1', 'I(x2^2)', TRUE, TRUE))}: Removes \code{I(x2^2)} if \code{x1} occurs in the model.
#' }
#' Note that inexact matching can remove terms incorrectly if inexact matches exist between names of terms or variables.  For example, if using an inexact match, then \code{verbotenCombos(list('x1', 'x2', FALSE, FALSE))} will find any term that has an \code{x1} (e.g., \code{x11}) and if it exists, remove any term with an \code{x2} (e.g., \code{x25}). Note that reciprocally removing predictors makes little sense since, for example \code{list(list('x1', 'x2', FALSE, FALSE), list('x2', 'x1', FALSE, FALSE))} removes all formulae with \code{x2} if \code{x1} appears then tries to find any models with \code{x2} that have \code{x1} (of which there will be none after the first set is removed).
#'
#' @examples
#'
#' makeFormulae(y ~ x1 + x2 + x3, maxTerms=3)
#' makeFormulae(y ~ x1 + x2 + x3, ia=FALSE, maxTerms=3)
#' verboten <- c('x1:x2', 'I(x1^2)')
#' makeFormulae(y ~ x1 + x2 + x3, verboten=verboten, maxTerms=3)
#'
#' makeFormulae(y ~ x1 + x2 + x3, maxTerms=3)
#' verbotenCombos <- list(list('x1', 'x2', TRUE, TRUE))
#' makeFormulae(y ~ x1 + x2 + x3, verbotenCombos=verbotenCombos, maxTerms=3)
#'
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
	returnFx=stats::as.formula,
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

			modelAsList <- apply(utils::combn(preds, countTerms), 2, as.list)
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

					modelAsList <- apply(utils::combn(preds, countTerms), 2, as.list)
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

				if (verbose) cat('Making formulae with quadratic and appropriate linear\n
								  terms plus linear terms that do not also appear as\n
								  quadratic terms...')

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
			models <- c(models, quadModels)

		}

	### models with 2-ways interactions
	###################################

		iaModels <- list()

		if (ia & length(preds) > 1) {

			# all models with 2-way interactions where linear terms also appear in interaction terms
			if (verbose) omnibus::say('Making formulae with two-way interaction and appropriate linear terms...')

			for (countTerms in 2:stopAt) {

				theseTerms <- utils::combn(preds, countTerms)

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
				if (verbose) omnibus::say('Making formulae with two-way interaction and appropriate linear terms plus\n   other linear terms that are not part of interaction terms...')

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
			if (!is.null(verboten) & length(iaQuadModels) > 0) iaQuadModels <- .removeVerbotenVariables(iaQuadModels, verboten=verboten)
			if (!is.null(verbotenCombos) & length(iaQuadModels) > 0) iaQuadModels <- .removeVerbotenVariableCombos(iaQuadModels, verbotenCombos=verbotenCombos)

			### add linear terms that may not be in combined quad and interaction models
			############################################################################

			if (verbose) omnibus::say('Making formulae with all possible two-way interaction, quadratic,\n    and appropriate linear terms plus linear terms not already in formula...')

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


#' Remove formula with unwanted terms.
#'
#' This function takes as an argument a list of character strings representing formulae and returns a potentially shortened list without formulae containing a certain term.
#' @param forms List of characters each representing a formula.
#' @param verboten Either \code{NULL} (default) in which case \code{forms} is returned without any manipulation. Alternatively, this is a character list of terms that are not allowed to appear in any model in \code{forms}. Models with these terms are removed from \code{forms}. Note that the order of variables in interaction terms does not matter (e.g., \code{x1:x2} will cause the removal of models with this term verbatim as well as \code{x2:x1}). All possible permutations of three-way interaction terms are treated similarly.
#' @return A list of character elements representing formulae.
#' @keywords internal
.removeVerbotenVariables <- compiler::cmpfun(function(
	forms,
	verboten
) {

	# ensure each variant of an interaction term is represented
	for (i in seq_along(verboten)) {
		if (grepl(verboten[i], pattern=':')) {

			splitTerms <- strsplit(verboten[i], split=':')[[1]]
			swappedTerms <- if (length(splitTerms) == 2) {
				paste0(splitTerms[2], ':', splitTerms[1])
			} else if (length(splitTerms) == 3) {
				c(
					paste0(splitTerms[1], ':', splitTerms[3], ':', splitTerms[2]),
					paste0(splitTerms[2], ':', splitTerms[1], ':', splitTerms[3]),
					paste0(splitTerms[2], ':', splitTerms[3], ':', splitTerms[1]),
					paste0(splitTerms[3], ':', splitTerms[1], ':', splitTerms[2]),
					paste0(splitTerms[3], ':', splitTerms[2], ':', splitTerms[1])
				)
			}

			verboten <- c(verboten, swappedTerms)

		}

	}

	verboten <- unique(verboten)

	# remove formulae with undesired term(s)
	removeThese <- numeric()
	for (countModel in seq_along(forms)) {

		if (any(forms[[countModel]] %in% verboten)) removeThese <- c(removeThese, countModel)

	}
	
	if (length(removeThese) > 0) forms <- rlist::list.remove(forms, removeThese)

	forms

})

#' Remove formula with unwanted combinations of variables.
#'
#' This function takes as an argument a list of character strings representing formulae and returns a potentially shortened list without formulae containing certain combinations of variables.
#' @param forms List of characters each representing a formula.
#' @param verbotenCombos Either \code{NULL} (default) in which case \code{forms} is returned without any manipulation. Alternatively, this argument can be used to specify variables or terms that should not occur in the same formula. The argument \code{verbotenCombos} is composed of a list of lists. Each sublist comprises names of two variables or terms stated as characters followed by two logical values (\code{TRUE}/\code{FALSE}). The second variable/term is removed from the model if the first is in the model. If the first logical value is \code{TRUE} then the second variable/term is removed if the first variable appears alone in the formula (e.g., not in an interaction with another variable). If the first logical value is \code{FALSE} then the second variable/term is removed if the first variable/term appears in any term (e.g., as an interaction with another term). 
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
#' @return A list of character elements representing formulae.
#' @keywords internal
.removeVerbotenVariableCombos <- compiler::cmpfun(function(
	forms,
	verbotenCombos
) {

	# stores indices of models to be removed
	removeThese <- numeric()

	for (countModel in seq_along(forms)) {

		for (countVerboten in seq_along(verbotenCombos)) {

			# is matching exact or not?
			exactMatch1 <- unlist(verbotenCombos[[countVerboten]])[3]=='TRUE'
			exactMatch2 <- unlist(verbotenCombos[[countVerboten]])[4]=='TRUE'
	
			# does the exact term appear in the formula
			logicalExact1 <- forms[[countModel]] %in% verbotenCombos[[countVerboten]][1]
			logicalExact2 <- forms[[countModel]] %in% verbotenCombos[[countVerboten]][2]
			
			exactTerm1 <- any(logicalExact1)
			exactTerm2 <- any(logicalExact2)
			
			# if so what is its index?
			exactIndex1 <- which(logicalExact1)
			exactIndex2 <- which(logicalExact2)
			
			# does a form of the term appear in the formula?
			logicalLoose1 <- grepl(forms[[countModel]], pattern=verbotenCombos[[countVerboten]][1])
			logicalLoose2 <- grepl(forms[[countModel]], pattern=verbotenCombos[[countVerboten]][2])
			
			looseTerm1 <- any(logicalLoose1)
			looseTerm2 <- any(logicalLoose2)
			
			# if so what is its index?
			looseIndex1 <- which(logicalLoose1)
			looseIndex2 <- which(logicalLoose2)
			
			# want exact matches for both terms
			if (exactMatch1 & exactMatch2) {
								
				if (exactTerm1 & exactTerm2) removeThese <- c(removeThese, countModel)
	
			# want exact match for first term, loose match for second
			} else if (exactMatch1 & !exactMatch2) {
	
				if (exactTerm1 & looseTerm2) removeThese <- c(removeThese, countModel)
				
			# want exact match for second term, loose match for first
			} else if (!exactMatch1 & exactMatch2) {
	
				if (looseTerm1 & exactTerm2) removeThese <- c(removeThese, countModel)
				
			# want loose match for both terms
			} else if (!exactMatch1 & !exactMatch2) {

				if (looseTerm1 & looseTerm2) removeThese <- c(removeThese, countModel)
				
			}
	
		} # next verbotenCombos list
	
	} # next model

	if (length(removeThese) > 0) forms <- rlist::list.remove(forms, removeThese)
	
	forms
	
})

#' Remove redundant model forms from a list of models
#'
#' This function takes as an argument a list of character vectors. Each set of character vectors represents terms in a formula, and each element of a specific term in that formula. It returns a possibly shortened list with vectors culled.
#' @param formList List of character variables each in formula format.
#' @return List.
#' @keywords internal
.removeRedundantModels <- compiler::cmpfun(function(
	formList
) {

	formList <- lapply(formList, FUN=function(x) sort(unique(x)))
	numTerms <- unlist(lapply(formList, length))
	
	keep <- list()
	
	for (thisTerms in unique(numTerms)) {
	
		listTheseTerms <- formList[which(numTerms==thisTerms)]
		listTheseTerms <- if (thisTerms==1) { matrix(sapply(listTheseTerms, paste), nrow=1) } else { sapply(listTheseTerms, paste) }
		
		newTerms <- rep('eq', ncol(listTheseTerms))
		
		for (countRow in 1:nrow(listTheseTerms)) newTerms <- paste(newTerms, listTheseTerms[countRow, ])
		
		uniqueTerms <- unique(listTheseTerms, MARGIN=2)
		
		for (countCol in 1:ncol(uniqueTerms)) keep[[length(keep) + 1]] <- c(uniqueTerms[ , countCol])
		
	}
	
	keep
	
})

