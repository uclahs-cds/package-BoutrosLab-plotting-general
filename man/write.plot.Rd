\name{write.plot}
\alias{write.plot}
\title{Simplifies plotting by standardizing and centralizing all output-handling}
\description{Handle various graphics-driver weirdness and writes an output file and returns 1 or returns the trellis.object}
\usage{
write.plot(trellis.object, filename = NULL, height = 6, width = 6, 
size.units = 'in', resolution = 1600, enable.warnings = FALSE, description = NULL);
}
\arguments{
    \item{trellis.object}{A trellis object to be plotted}
    \item{filename}{Filename for output, or if NULL (default value) returns the trellis object itself. Will automatically grab the extension used}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to 'in'}
    \item{resolution}{Figure resolution, defaults to 1000}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{description}{Short description of image; default NULL}
}
\value{Returns a list with components base and exponent. Mostly for use in embedding scientific notation in a plot.}
\author{Paul C. Boutros}
\keyword{iplot}
