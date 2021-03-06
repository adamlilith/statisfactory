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
#' @examples
#' forms <- list()
#' forms[[1]] <- 'y ~ x1 + x2 + x3'
#' forms[[2]] <- 'y ~ x1 + x2 + x3 + I(x1^2)'
#' forms[[3]] <- 'y ~ x1 + x2 + x3 + x1:x2'
#' forms[[4]] <- 'y ~ x1 + x2 + x3 + x1:x2 + I(x1^2)'
#' forms[[5]] <- 'y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3'
#'
#' verbotenCombos <- list(list('x1', 'x2', TRUE, TRUE))
#' .removeVerbotenVariableCombos(forms, verbotenCombos)
#'
#' verbotenCombos <- list(list('x1', 'x2', FALSE, TRUE))
#' .removeVerbotenVariableCombos(forms, verbotenCombos)
#'
#' verbotenCombos <- list(list('x1', 'x2', TRUE, FALSE))
#' .removeVerbotenVariableCombos(forms, verbotenCombos)
#'
#' verbotenCombos <- list(list('x1', 'x2', FALSE, FALSE))
#' .removeVerbotenVariableCombos(forms, verbotenCombos)
#'
#' verbotenCombos <- list(list('x1:x3', 'x2', TRUE, TRUE))
#' .removeVerbotenVariableCombos(forms, verbotenCombos)
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
	
				if (looseFirstTerm & exactTerm2) removeThese <- c(removeThese, countModel)
				
			# want loose match for both terms
			} else if (!exactMatch1 & !exactMatch2) {

				if (looseTerm1 & looseTerm2) removeThese <- c(removeThese, countModel)
				
			}
	
		} # next verbotenCombos list
	
	} # next model

	if (length(removeThese) > 0) forms <- rlist::list.remove(forms, removeThese)
	
	forms
	
})
