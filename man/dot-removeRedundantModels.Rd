% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeFormulae.r
\name{.removeRedundantModels}
\alias{.removeRedundantModels}
\title{Remove redundant model forms from a list of models}
\usage{
.removeRedundantModels(formList)
}
\arguments{
\item{formList}{List of character variables each in formula format.}
}
\value{
List.
}
\description{
This function takes as an argument a list of character vectors. Each set of character vectors represents terms in a formula, and each element of a specific term in that formula. It returns a possibly shortened list with vectors culled.
}
\keyword{internal}
