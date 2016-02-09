\name{colour}
\alias{gleason.score.colour}
\alias{gleason.sum.colour}
\alias{age.gradient.colour}
\alias{clinicalT.3category.colour}
\alias{clinicalT.9category.colour}
\alias{psa.categorical.colour}
\alias{psa.gradient.colour}

\title{Provide covariate colour schemes for various annotions.}
\description{Returns the standardized colours given a set of annotations in char format}
\usage{
gleason.score.colour(x);
}
\arguments{
  \item{x}{List or vector of characters, corresponding to that annotation function.}
}
\author{Richard de Borja}
\examples{
# vector
gleason.vector <- c('4+3','3+4');
gleason.score.colour(gleason.vector);
}
\keyword{colour}
\keyword{scheme}
