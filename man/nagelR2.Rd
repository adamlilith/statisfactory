% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nagelR2.r
\name{nagelR2}
\alias{nagelR2}
\title{Nagelkerge's / Craig & Uhler's R2}
\usage{
nagelR2(likeNull, likeFull, n)
}
\arguments{
\item{likeNull}{Likelihood (not log-likelihood) of the null model or an object of class \code{logLik} with log-likelihood of the null model (usually an intercept-only model).}

\item{likeFull}{Likelihood (not log-likelihood) of the "full" model or an object of class \code{logLik} with log-likelihood of the "full" model (usually a model with covariates).}

\item{n}{Sample size.}
}
\value{
Numeric.
}
\description{
Nagelkerge's / Craig & Uhler's R2
}
\examples{

# create data
x <- 1:100
y <- 2 + 1.7 * x + rnorm(100, 0, 30)

# models
nullModel <- lm(y ~ 1)
fullModel <- lm(y ~ x)

# plot
plot(x, y)
abline(nullModel, col='red')
abline(fullModel, col='blue')
legend('bottomright', legend=c('Null', 'Full'), lwd=1, col=c('red', 'blue'))

# R2
likeNull <- exp(as.numeric(logLik(nullModel)))
likeFull <- exp(as.numeric(logLik(fullModel)))
nagelR2(likeNull, likeFull, 100)
}
