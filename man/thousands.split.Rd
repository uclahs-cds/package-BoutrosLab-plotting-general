\name{thousands.split}
\alias{thousands.split}
\title{Divide strings into groups of thousands}
\description{Takes a single number or list, and converts them into a new string with commas to mark the thousand multiples}
\usage{
thousands.split(
        nums
)
}
\arguments{
    \item{nums}{The numbers to be divided}
}
\author{Jeffrey Green}
\examples{

thousands.split(2344)

nums = c(1,2,34343,56565645645,676756,3434)

thousands.split(nums)

scatter.data <- data.frame(
    sample.one = microarray[1:800,1],
    sample.two = microarray[1:800,2],
    chr = microarray$Chr[1:800]
    );

create.scatterplot(
    # filename = tempfile(pattern = 'Test_Divide_Thousands', fileext = '.tiff'),
    formula = sample.two ~ sample.one,
    data = scatter.data,
    main = 'Axes & Labels',
    # Axes and labels
    xlab.label = colnames(microarray[1]),
    ylab.label = colnames(microarray[2]),
    yaxis.lab = thousands.split(c(1,2323,4545,567676,454,76767678678,89,787)),
    xat = seq(0, 16, 2),
    yat = seq(0, 16, 2),
    xlimits = c(0, 15),
    ylimits = c(0, 15),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    description = 'Scatter plot created by BoutrosLab.plotting.general'
    );
}

