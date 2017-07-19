\name{pcawg.colours}
\alias{pcawg.colours}
\alias{pcawg.colors}
\title{Return standard PCAWG colour palettes.}
\description{Return standard PCAWG colour palettes. Case insensitive.}
\usage{
pcawg.colours(
	x = NULL, 
	scheme = NULL, 
	fill.colour = 'slategrey', 
	return.scheme = FALSE);
}
\arguments{
    \item{x}{Chracter vector with terms to be mapped to colours. Ignored if scheme='all' or return.scheme=TRUE.}
    \item{scheme}{String specifying desired colour scheme. To see all available schemes, use scheme='all', returns.scheme=FALSE.}
    \item{fill.colour}{Unrecognized output will be filled with this colour. Default to 'slategrey'.}
    \item{return.scheme}{TRUE/FALSE. Set to true to return full specified scheme. Set to false to map x to colours.}
    }
\details{For further information on colour schemes, refer to the plotting guide.) 
}
\author{Jennifer Aguiar & Constance Li}
\keyword{colour}
\keyword{scheme}
