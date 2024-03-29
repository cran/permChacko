% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permChacko.R
\name{permChacko}
\alias{permChacko}
\title{The Chacko test for order-restriction with permutation test}
\usage{
permChacko(x, n_perm = 1000L, verbosity = 0)
}
\arguments{
\item{x}{vector of numeric values}

\item{n_perm}{number of permutations to calculate the p-value numerically}

\item{verbosity}{if \code{TRUE}, prints intermediate messages and output}
}
\value{
A list containing the test statistic, p-values (analytic, numeric and
tabular, when available), the number of permutations performed, the original
data and the reduced data. Use \code{\link[=names]{names()}} and \code{\link[=str]{str()}} on the output for more
details.
}
\description{
The Chacko test for order-restriction with permutation test
}
\examples{
ruxton221207 <- c(6, 8, 4, 7, 3)
chacko66_sec3 <- c(10L, 16L, 14L, 12L, 18L)
chacko66_sec5 <- c(12L, 14L, 18L, 16L, 22L, 20L, 18L, 24L, 26L, 30L)

permChacko(ruxton221207)
permChacko(chacko66_sec3)
permChacko(chacko66_sec5)
}
\references{
Chacko, V. J. (1963). Testing homogeneity against ordered alternatives. The
Annals of Mathematical Statistics, 945-956.

Chacko, V. J. (1966). Modified chi-square test for ordered alternatives.
Sankhyā: The Indian Journal of Statistics, Series B, 185-190.
}
