\name{append.footnote}
\alias{append.footnote}
\title{Appends Footnotes to Grid Objects}
\description{Users can add small footnotes in the corners of their plots}
\usage{
append.footnote(
	text = Sys.Date(), 
	size = 0.5, 
	colour = 'black', 
	position = "bottomright"
	);
}
\arguments{
    \item{text}{The footnote text.}
    \item{size}{The size of the footnote text.}
    \item{colour}{The colour of the footnote text.}
    \item{position}{The location to append the footnote, relative to the plot. Options: topleft, topright, bottomleft, bottomright, middle. Default is bottomright.}
    }
\author{Clement Fung}
\examples{
# TBC
}
