\name{display.colours}
\alias{display.colours}
\alias{display.colors}
\title{Function to display R colors, as well as corresponding R grey colours.}
\description{Displays R colors and their corresponding R grey colours.}
\usage{
display.colours(
	cols, 
	names = cols
	);
}
\arguments{
    \item{cols}{Vector of colours to be displayed.}
    \item{names}{The names of the colours. Defaults to equal the input of cols}
    }
\details{For further information on colour schemes, refer to the colour guide (in Resources/general) 
}
\author{Christine P'ng}
\examples{

display.colours('red');
# Red and Grey are displayed

display.colours(default.colours(5));
# Five default colours and their grey values are displayed

test.colours <- force.colour.scheme(c('skin','nerve'), 'tissue');
display.colours(test.colours);

}
\keyword{colour}
\keyword{scheme}
\keyword{grey}
