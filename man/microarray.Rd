\name{microarray}
\alias{microarray}
\docType{data}
\title{Microarray dataset of colon cancer patients}

\description{
    Gene expression level changes of 2382 genes across 58 colon cancer patients. 
    Additional data on the genes include chromosomal location and p-values.
    Additional data on the patient samples is found in in the "\code{patient}" dataset.
    The same patient samples are described in the "\code{SNV}" and "\code{CNA}" datasets.
}

\usage{microarray}

\format{
    A data frame with 62 columns and 2383 rows. Columns 1-58 indicate the cancer 
    patient sample. Columns 59-61 indicate the (sorted) chromosomal location by
    "\code{Chr}", "\code{Start}", and "\code{End}". Column 62 contains adjusted 
    p-values. Each row is a different gene, and the row names are the gene names.
}

\author{Christine P'ng}

\examples{
create.dotmap(
    filename = tempfile(pattern = 'Using_microarray_dataset', fileext = '.tiff'),
    x = microarray[1:5,1:5],
    main = 'microarray data',
    spot.size.function = function(x) {abs(x)/3;},
    xaxis.cex = 0.8,
        yaxis.cex = 0.8,
        xaxis.rot = 90,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );
}

\keyword{datasets}
