\name{CNA}
\alias{CNA}
\docType{data}
\title{Copy number aberration (CNA) data from colon cancer patients}

\description{
    CNA calls from 30 genes across 58 colon cancer patients.
    Additional data on the patient samples is found in the \code{patient} dataset.
    The same patient samples are described in the \code{microarray} and \code{SNV} 
    datasets.
}

\usage{CNA}

\format{
    A data frame with 58 columns and 30 rows. The columns indicate the patient sample,
    and the rows indicate the gene. The contents of the data frame are encoded such that
    0 indicates no CNA, -1 indicates a CNA loss, and 1 indicates a CNA gain.
}

\author{Christine P'ng}

\examples{
data(CNA);
create.dotmap(
    # filename = tempfile(pattern = 'Using_CNA_dataset', fileext = '.tiff'),
    x = CNA[1:15, 1:15],
    main = 'CNA data',
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.rot = 90,
    description = 'Dotmap created by BoutrosLab.plotting.general',
    resolution = 50
    );
}

\keyword{datasets}
