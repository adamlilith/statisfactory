#' Remove formula with unwanted terms.
#'
#' This function takes as an argument a list of character strings representing formulae and returns a potentially shortened list without formulae containing a certain term.
#' @param forms List of characters each representing a formula.
#' @param verboten Either \code{NULL} (default) in which case \code{forms} is returned without any manipulation. Alternatively, this is a character list of terms that are not allowed to appear in any model in \code{forms}. Models with these terms are removed from \code{forms}. Note that the order of variables in interaction terms does not matter (e.g., \code{x1:x2} will cause the removal of models with this term verbatim as well as \code{x2:x1}). All possible permutations of three-way interaction terms are treated similarly.
#' @examples
#' forms <- list()
#' forms[[1]] <- 'y ~ x1 + x2 + x3'
#' forms[[2]] <- 'y ~ x1 + x2 + x3 + I(x1^2)'
#' forms[[3]] <- 'y ~ x1 + x2 + x3 + x1:x2'
#' forms[[4]] <- 'y ~ x1 + x2 + x3 + x1:x2 + I(x1^2)'
#' forms[[5]] <- 'y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3'
#' forms[[6]] <- 'y ~ x1 + x2 + x3 + x1:x2:x3'
#'
#' verboten <- 'x1'
#' .removeVerbotenVariables(forms, verboten)
#'
#' verboten <- c('x1', 'x2')
#' .removeVerbotenVariables(forms, verboten)
#'
#' verboten <- 'x1:x2'
#' .removeVerbotenVariables(forms, verboten)
#'
#' verboten <- 'I(x1^2)'
#' .removeVerbotenVariables(forms, verboten)
#'
#' verboten <- 'x3:x2:x1'
#' .removeVerbotenVariables(forms, verboten)
#'
#' @return A list of character elements representing formulae.

.removeVerbotenVariables <- function(
	forms,
	verboten
) {

	# ensure each variant of an interaction term is represented
	for (i in seq_along(verboten)) {
		if (grepl(verboten[i], pattern=':')) {
			
			splitTerm <- strsplit(verboten[i], split=':')[[1]]
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
	for (countModel in seq_along(forms)) {

		if (any(forms[[countModel]] %in% verboten)) forms[[countModel]] <- NULL
		
	}
	
	forms
	
}
