#' Remove redundant model forms from a list of models
#'
#' This function takes as an argument a list of character vectors. Each set of character vectors represents terms in a formula, and each element of a specific term in that formula. It returns a possibly shortened list with vectors culled.
#' @param formList List of character variables each in formula format.
#' @return List.
.removeRedundantModels <- function(formList) {

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
	
}
