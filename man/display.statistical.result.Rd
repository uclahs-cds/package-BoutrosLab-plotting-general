\name{display.statistical.result}
\alias{display.statistical.result}
\title{Utility function to display statistical result in a plot}
\description{A utility function to display statistical result in a plot in scientific notation (when appropriate)}
\usage{
display.statistical.result(
	x,
	lower.cutoff = 2.2e-50,
	scientific.cutoff = 0.001,
	digits = 2,
	statistic.type = 'P',
	symbol = ': '
	);
}
\arguments{
    \item{x}{Numeric value to be displayed}
    \item{lower.cutoff}{For values of x smaller than lower.cutoff, the return value will be "< lower.cutoff".  Defaults to 2.2e-16}
    \item{scientific.cutoff}{For values of x larger or equal to scientific.cutoff, standard notation will be used (rather than scientific notation).  Defaults to 0.001 }
    \item{digits}{Number of decimal places of precision to be shown}
    \item{statistic.type}{Type of statistic to be displayed, defaults to \dQuote{P}.}
    \item{symbol}{Symbol prior to statistic to be displayed, defaults to \dQuote{: }.}
}

\value{Returns an expression}
\author{Nathalie Moon}
\seealso{\code{scientific.notation}}
\examples{
set.seed(100);

display.statistical.result(x = 0.00000000000000000000234);
display.statistical.result(x = 0.023, statistic.type = 'Q');
display.statistical.result(x = 0.001, scientific.cutoff = 0.01, symbol = ' = ');

}
\keyword{hplot}


