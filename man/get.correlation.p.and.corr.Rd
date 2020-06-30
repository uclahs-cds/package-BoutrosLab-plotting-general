\name{get.correlation.p.and.corr}
\alias{get.correlation.p.and.corr}
\title{Calculate a correlation and its statistical significance}
\description{Returns the correlation and p-value for two variables using a user-specified correlation metric. P-values are estimated analytically, not via permutation-testing.}
\usage{
get.correlation.p.and.corr(x, y, alternative = 'two.sided', method = 'pearson');
}
\arguments{
  \item{x}{Vector of numbers to analyze}
  \item{y}{Vector of numbers to analyze}
  \item{alternative}{What is the null-hypothesis?}
  \item{method}{The correlation technique to use (passed directly to cor.test)}
}
\value{Returns a two-element vector containing the correlation and its p-value.}
\author{Paul C. Boutros}
\examples{
get.correlation.p.and.corr(
	x = runif(100),
	y = runif(100),
	method = 'pearson'
	);

get.correlation.p.and.corr(
	x = sample(1:10, 100, replace = TRUE),
	y = runif(100),
	method = 'spearman'
	);
}
\keyword{htest}
