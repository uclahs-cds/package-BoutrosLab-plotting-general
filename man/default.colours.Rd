\name{default.colours}
\alias{default.colours}
\alias{default.colors}
\title{Provides default colour schemes.}
\description{Returns colour schemes based on user input. Used to provide default colour schemes for simple cases.}
\usage{
default.colours(
	number.of.colours = 2,
	palette.type = 'qual',
	is.greyscale = TRUE,
	is.venn = FALSE
	);
}
\arguments{
    \item{number.of.colours}{The number of colours requested for the colour scheme.}
    \item{palette.type}{The type of colour scheme requested. Only palette types of \dQuote{seq}, \dQuote{div}, \dQuote{qual}, \dQuote{pastel}, \dQuote{survival}, \dQuote{dotmap}, \dQuote{spiral.sunrise}, \dQuote{spiral.morning}, \dQuote{spiral.dusk}, \dQuote{spiral.noon}, \dQuote{spiral.afternoon}, \dQuote{spiral.dawn}, and \dQuote{spiral.night} are accepted. Legacy colour palettes are available under \dQuote{chromosomes}, \dQuote{old.qual1}, \dQuote{old.qual2}, \dQuote{old.seq}, and \dQuote{old.div}. \dQuote{seq} corresponds to sequential colour schemes, \dQuote{div} corresponds to diverging colour schemes, and \dQuote{qual} corresponds to qualitative colour schemes - \dQuote{pastel} is a pastel version of this palette. \dQuote{survival} is useful for survival plots, as the first two colour are blue and red, following convention. The remaining colour schemes are not tied to a specific use-case.}
    \item{is.greyscale}{Boolean asking whether or the colour scheme should be greyscale-compatible. Defaults to TRUE. The purpose of this parameter is to warn users if they ask for a colour scheme that is not greyscale-compatible. Regardless of the value of is.greyscale, the same colour scheme will be provided.}
    \item{is.venn}{Boolean determining whether or not the colour scheme is to be used for a venn diagram. If TRUE, the palette type should be set to NULL. For venn diagrams, text colours are also provided.}
    }
\details{For further information on colour schemes, refer to the plotting guide.) 
}
\author{Christine P'ng}
\examples{

default.colours(number.of.colours = 6, is.greyscale = FALSE, palette.type = 'div')
# Returns:
# [1] "#B32B2B" "#DD4E4E" "#EB7C7C" "#F7BEBE" "#BEF4F7" "#80CDD1"

default.colours(number.of.colours = 3, palette.type = NULL, is.venn = TRUE)
# Returns:
# [1] "red"        "dodgerblue" "yellow"    
# [1] "darkred"    "darkblue"   "darkorange"
# The second line of colours is the corresponding text colour

default.colours(number.of.colours = c('2','5','3'), c('binary','seq','seq'))
# Returns:
# [[1]]
# [1] "white"       "chartreuse3"

# [[2]]
# [1] "lavenderblush"  "pink"           "palevioletred1" "violetred1"    
# [5] "maroon"        

# [[3]]
# [1] "aliceblue"    "lightblue1"   "lightskyblue"

default.colours(5, 'spiral.sunrise');
# Returns: 
# [1] "#336A90" "#65B4A2" "#B1D39A" "#F4E0A6" "#FFE1EE"

}
\keyword{colour}
\keyword{scheme}
