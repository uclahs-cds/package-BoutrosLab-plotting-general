\name{SNV}
\alias{SNV}
\docType{data}
\title{Single nucleotide variant (SNV) data from colon cancer patients}

\description{
    SNV calls from 30 genes across 58 colon cancer patients.
    Additional data on the patient samples is found in in the "\code{patient}" dataset.
    The same patient samples are described in the "\code{microarray}" and "\code{CNA}" 
    datasets.
}

\usage{SNV}

\format{
    A data frame with 58 columns and 30 rows. The columns indicate the patient sample,
    and the rows indicate the gene. The contents of the data frame are either NA (indicating no
    SNV call was made) or one of: 
    \itemize{
        \item{1 - nonsynonymous SNV}
        \item{2 - stopgain SNV}
        \item{3 - frameshift insertion}
        \item{4 - frameshift deletion}
        \item{5 - nonframeshift insertion}
        \item{6 - nonframeshift deletion}
        \item{7 - splicing}
        \item{8 - unknown}
    }
}

\author{Christine P'ng}

\examples{
len <- apply(SNV[1:15], 2, function(x){mutation.count <- length(which(x == 1))});

create.barplot(
    # filename = tempfile(pattern = 'Using_SNV_dataset', fileext = '.tiff'),
    formula = len ~ colnames(SNV[1:15]) ,
    data = SNV,
    main = 'SNV dataset',
    xaxis.rot = 45, 
    ylimits = c(0,30),
    yat = seq(0,30,5),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );
}
\keyword{datasets}
