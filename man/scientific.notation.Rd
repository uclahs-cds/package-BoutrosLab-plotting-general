\name{scientific.notation}
\alias{scientific.notation}
\title{Use scientific notation in plots}
\description{Returns an expression or list for plotting data in scientific notation}
\usage{
scientific.notation(
	x, 
	digits = 1, 
	type = 'expression'
	);
}
\arguments{
    \item{x}{The number we want in scientific notation.}
    \item{digits}{How many decimal places to keep.}
    \item{type}{The format to return the value in. Defaults to 'expression', also accepts 'list'}
}
\value{Generates scientific notation either as an expression or list.}
\author{Paul C. Boutros}
\keyword{iplot}
