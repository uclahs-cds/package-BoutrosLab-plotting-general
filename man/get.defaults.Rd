\name{get.defaults}
\alias{get.defaults}
\title{Get operating system specific default properties}
\description{Returns the value for the property requested}
\usage{
get.defaults(
	property = 'fontfamily',
	os.type = .Platform$OS.type,
	add.to.list = NULL,
	use.legacy.settings = FALSE
	);
}
\arguments{
    \item{property}{The property to be retrieved}
    \item{os.type}{operating system (optional). valid values are: \dQuote{windows}, \dQuote{unix}}
    \item{add.to.list}{appends the requested property to this parameter}
    \item{use.legacy.settings}{boolean to set wheter or not to use legacy mode settings (font)}
}
\value{Returns the value (list if add.to.list is passed) for the property requested given the os.type parameter. If latter is missing, it attempts to find user's operating system}
\author{Syed Haider}
\examples{
    # returns the fontfamily for current OS
    get.defaults(property = "fontfamily");

    # returns the fontfamily for unix
    get.defaults(property = "fontfamily", os.type = 'windows');
}
\keyword{iplot}
