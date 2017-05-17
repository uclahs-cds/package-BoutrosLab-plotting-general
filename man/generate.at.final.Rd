\name{generate.at.final}
\alias{generate.at.final}
\title{Generates alternative default tick mark locations for create.densityplot() and create.scatterplot()}
\description{Generates the tick mark locations for the output graphic of create.densityplot(), based on the values to the arguments of that function.  This is needed to ensure the grid line and tick mark locations agree with each other.} 
\usage{
generate.at.final(
	at.input,
	limits,
	data.vector
	);
}
\arguments{
    \item{at.input}{either a logical scalar or a numeric vector}
    \item{limits}{either NULL or a numeric vector of length 2}
    \item{data.vector}{a numeric vector}
}
\value{Returns a numeric vector containing the tick mark locations of the densityplot.}
\author{Kenneth C.K. Chu}
\keyword{iplot}
