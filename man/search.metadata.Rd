\name{search.metadata}
\alias{search.metadata}
\title{Function to search for photos based on their metadata}
\description{Input several search parameters for metadata and get a list returning the names of files matching the parameters}
\usage{
search.metadata(
	path, 
	FileName = NULL,
	Description = NULL,
	BLPlottingVersion = NULL,
	Author = NULL
	);
}
\arguments{
    \item{path}{path for files to look through}
    \item{FileName}{File name to look for}
    \item{Description}{Description to look for}
    \item{BLPlottingVersion}{Plotting General version to look for}
    \item{Author}{Author to look for}
    }

\author{Jeff Green}
\examples{
search.metadata(".",FileName = "test", Description = "Test", BLPlottingVersion = "5.3.7", Author = "jgreen")
}
\keyword{search}
\keyword{metadata}
