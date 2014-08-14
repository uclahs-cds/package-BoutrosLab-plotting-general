\name{colour.gradient}
\alias{colour.gradient}
\title{Creates a colour gradient}
\description{Creates a sequential palette of colours.}
\usage{
colour.gradient(colour, length);
}
\arguments{
    \item{colour}{A single colour to be used as the center value of the sequence}
    \item{length}{The number of colours to include in the palette}
    }

\author{Ren Sun & Christine P'ng}
\examples{

display.colours(colour.gradient('dodgerblue2', 6));
# Colours displayed:
#[1] "#BEDCFA" "#7DB9F5" "#3C97F0" "#1872CC" "#104C88" "#082644"

display.colours(colour.gradient(default.colours(1), 3));
# Colours displayed:
#[1] "#BFBFBF" "#7F7F7F" "#3F3F3F"

}
\keyword{colour}
\keyword{scheme}
\keyword{RGB}
\keyword{grey}
