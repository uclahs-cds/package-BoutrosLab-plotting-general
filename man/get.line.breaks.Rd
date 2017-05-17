\name{get.line.breaks}
\alias{get.line.breaks}
\title{Get line breaks}
\description{Given a vector, returns the indices (and an adjustment to draw lines between cells) where the value is not equal to the preceding value. Main use intended to be in row.lines arguments to create.heatmap}
\usage{
get.line.breaks(
	x
	);
}
\arguments{
    \item{x}{A vector, numeric, factor or character.}
 }
\value{A vector of integers representing the break points in the vector x}
\author{Nicholas Harding}
\examples{
set.seed(12345);
values <- sample(
	default.colours(3),
	20,
	replace = TRUE
	);
get.line.breaks(values);
}
