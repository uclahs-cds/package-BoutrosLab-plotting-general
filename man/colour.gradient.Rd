\name{colour.gradient}
\alias{colour.gradient}
\alias{color.gradient}
\title{Creates a colour gradient}
\description{Creates a sequential palette of colours.}
\usage{
colour.gradient(
	colour, 
	length
	);
}
\arguments{
    \item{colour}{A single colour to be used as the center value of the sequence}
    \item{length}{The number of colours to include in the palette}
    }

\author{Ren Sun & Christine P'ng}
\examples{

display.colours(colour.gradient('dodgerblue2', 6));

display.colours(colour.gradient(default.colours(1), 3));

}
\keyword{colour}
\keyword{scheme}
\keyword{RGB}
\keyword{grey}
