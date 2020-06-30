\name{write.plot}
\alias{write.plot}
\title{Simplifies plotting by standardizing and centralizing all output-handling}
\description{Handle various graphics-driver weirdness and writes an output file and returns 1 or returns the trellis.object}
\usage{
write.plot(
	trellis.object,
	filename = NULL,
	additional.trellis.objects = NULL,
	additional.trellis.locations = NULL,
	height = 6,
	width = 6,
	size.units = 'in',
	resolution = 1000,
	enable.warnings = FALSE,
	description = "Created with BoutrosLab.plotting.general"
	);
}
\arguments{
    \item{trellis.object}{A trellis object to be plotted}
    \item{filename}{Filename for output, or if NULL (default value) returns the trellis object itself. 
	Will automatically grab the extension used.}
    \item{additional.trellis.objects}{List of additional trellis objects to add to main	plot. Default to NULL}
    \item{additional.trellis.locations}{List of coordinates for additional trellis objects. 
	Must be represented using variable names 'xleft', 'ybottom', 'xright' and 'ytop'. Defaults to NULL}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to 'in'}
    \item{resolution}{Figure resolution, defaults to 1000}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{description}{Short description of image; default NULL}
}
\value{Returns the trellis.object if filename is NULL or writes the plot to file if a filename is specified.}
\author{Paul C. Boutros}
\keyword{iplot}
\examples{
set.seed(253647)
# create test data
tmp.data <- data.frame(
    x = c(
        runif(n = 150, min =  0, max = 20),
        runif(n = 150, min = 40, max = 60),
        runif(n = 700, min =  0, max = 40)
        ),
    y = c(
        runif(n = 150, min =  0, max =  20),
        runif(n = 150, min = 40, max = 60),
        runif(n = 700, min =  0, max = 40)
        )
    );

main.plot <- create.densityplot(
    x = list(
        X = tmp.data$x,
        Y = tmp.data$y
        ),
    xlab.label = 'X Axis Title',
    ylab.label = 'Y Axis Title',
    xlimits = c(-50,150),
    ylimits = c(0,0.03),   
    xat = seq(-50,150,50),
    yat = seq(0,0.03,0.005),
    description = 'Image description goes here'
    );

secondary.plot <- create.densityplot(
    x = list(
        X = tmp.data$x,
        Y = tmp.data$y
        ),
    xlab.label = '',
    ylab.label = '',
    xlimits = c(50,75),
    ylimits = c(0,0.015),   
    xat = seq(0,150,10),
    yat = seq(0,0.015,0.005),
    xaxis.tck = 0,
    description = 'Image description goes here'
    );

write.plot(
    filename = tempfile(pattern = 'write_plot_example', fileext = '.tiff'),
    trellis.object = main.plot,
    additional.trellis.objects = list(secondary.plot),
    additional.trellis.locations = list(
        xleft = 0.6,
        ybottom = 0.5,
        xright =0.97,
        ytop = 0.9
        ),
    resolution = 50 # Lowering resolution decreases file size
    );
}
