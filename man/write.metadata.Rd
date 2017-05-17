\name{write.metadata}
\alias{write.metadata}
\title{Writes Metadata}
\description{Utilizes exiftool to write metadata to generated plots. Writes the R version, lattice version, latticeExtra version, BoutrosLab.plotting.general version, BoutrosLab.plotting.survival version, operating system, machine, author, image description.}
\usage{
write.metadata(
	filename = NULL,
	description = NULL,
	verbose = FALSE
	);
}
\arguments{
	\item{filename}{Filename for output, or if NULL (default value) returns image unchanged.}
	\item{description}{Short description of image; default NULL}
	\item{verbose}{Option to standard output; default FALSE}
}
\value{If \code{filename} is \code{NULL}, returns the image unchanged. If \code{description} is \code{NULL}, then the image is returned without the description tag. 

Note: an easy way to view the metadata is by using the exiftool command.
}
\author{Esther Jung}
\keyword{write}
