\name{show.available.palettes}
\alias{show.available.palettes}
\title{Display the available colour palettes}
\description{Displays the available colour palettes}
\usage{
show.available.palettes(type = 'general', filename = NULL, height = 8, width = 12, 
	resolution = 300);
}
\arguments{
    \item{type}{Either \dQuote{general}, \dQuote{specific}, or \dQuote{both} (default)}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{height}{Figure height, defaults to 8 inches -- this is optimal for the specific schemes}
    \item{width}{Figure width, defaults to 12 -- this is optimal for the specific schemes}
    \item{resolution}{Figure resolution in dpi, defaults to 300}
    }

\author{Christine P'ng}
\examples{

show.available.palettes(type = 'specific', filename = 'show_case_specific_schemes.tiff');

show.available.palettes(type = 'general', filename = 'default_schemes.tiff', height = 6, width = 8)
}
\keyword{colour}
\keyword{scheme}
\keyword{RGB}
\keyword{grey}