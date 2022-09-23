\name{create.histogram}
\alias{create.histogram}
\title{Make a histogram}
\description{Takes a vector and creates a histogram}
\usage{
create.histogram(
	x,
	data,
	filename = NULL,
	main = NULL,
	main.just = 'center',
	main.x = 0.5,
	main.y = 0.5,
	main.cex = 3,
	xlab.label = NULL,
	ylab.label = NULL,
	xlab.cex = 2,
	ylab.cex = 2,
	xlab.col = 'black',
	ylab.col = 'black',
	xaxis.lab = TRUE,
	yaxis.lab = TRUE,
	xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.col = 'black',
	yaxis.col = 'black',
	xaxis.tck = 1,
	yaxis.tck = 1,
	xaxis.fontface = 'bold',
	yaxis.fontface = 'bold',
	xlab.top.label = NULL,
	xlab.top.cex = 2,
	xlab.top.col = 'black',
	xlab.top.just = 'center',
	xlab.top.x = 0.5,
	xlab.top.y = 0,
	type = 'percent',
	breaks = NULL,
	col = 'white',
	border.col = 'black',
	lwd = 2,
	lty = 1,
	layout = NULL,
	x.spacing = 0,
	y.spacing = 0,
	x.relation = 'same',
	y.relation = 'same',
	strip.col = 'white',
	strip.cex = 1,
	top.padding = 0.1,
	bottom.padding = 0.7,
	right.padding = 0.1,
	left.padding = 0.5,
	ylab.axis.padding = 0,
	abline.h = NULL,
	abline.v = NULL,
	abline.col = 'black',
	abline.lwd = 1,
	abline.lty = 1,
	key = NULL,
	legend = NULL,
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	height = 6,
	width = 6,
	size.units = 'in',
	resolution = 1600,
	enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab',
	preload.default = 'custom',
        use.legacy.settings = FALSE,
	inside.legend.auto = FALSE
	);
}
\arguments{
    \item{x}{A formula or a numeric vector (not frequencies!)}
    \item{data}{An optional data source if x is a formula}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of text for main plot title, defaults to 2}
    \item{xlab.label}{x-axis title}
    \item{ylab.label}{y-axis title}
    \item{xlab.cex}{Size of x-axis label, defaults to 2}
    \item{ylab.cex}{Size of y-axis label, defaults to 2}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 1}
    \item{yaxis.cex}{Size of y-axis tick labels, defaults to 1}
    \item{xlimits}{Two-element vector giving the x-axis limits}
    \item{ylimits}{Two-element vector giving the y-axis limits}
    \item{xat}{Vector listing where the x-axis ticks should be drawn}
    \item{yat}{Vector listing where the y-axis ticks should be drawn}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xlab.top.label}{The label for the top x-axis}
    \item{xlab.top.cex}{Size of top x-axis label}
    \item{xlab.top.col}{Colour of the top x-axis label}
    \item{xlab.top.just}{Justification of the top x-axis label, defaults to centered}
    \item{xlab.top.x}{The x location of the top x-axis label}
    \item{xlab.top.y}{The y location of the top y-axis label}
    \item{type}{Should the plot be of the \dQuote{percent} (default), \dQuote{density} or \dQuote{count}}
    \item{breaks}{A vector listing the break-points of the histogram, or an integer specifying the desired number of breaks.}
    \item{col}{Fill colour for the histograms}
    \item{border.col}{Specify border colour (defaults to black)}
    \item{lwd}{Specifies line width}
    \item{lty}{Specifies line style}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}.  
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.1}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0.7}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0.5}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0.5}
    \item{ylab.axis.padding}{A number specifying the distance of ylabel to the y-axis, defaults to 0}, 
    \item{abline.h}{Allow horizontal line to be drawn, default to NULL}
    \item{abline.v}{Allow vertical line to be drawn, default to NULL}
    \item{abline.col}{Horizontal and vertical line colour, defaults to black}
    \item{abline.lwd}{Specifies horizontal/vertical line width, defaults to 1}
    \item{abline.lty}{Specifies horizontal/vertical line style, defaults to 1 (solid)}
    \item{key}{Add a key to the plot. See xyplot.}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x ooordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{description}{Short description of image/plot; default NULL.}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
    \item{preload.default}{ability to set multiple sets of diffrent defaults depending on publication needs}
    \item{use.legacy.settings}{boolean to set wheter or not to use legacy mode settings (font)}
    \item{inside.legend.auto}{boolean specifying whether or not to use the automatic inside legend function}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\author{Paul C. Boutros}
\section{Warning}{
If this function is called without capturing the return value, or specifying a filename, it may crash while trying to draw the histogram. In particular, if a script that uses such a call of create histogram is called by reading the script in from the command line, it will fail badly, with an error message about unavailable fonts:\cr
        \preformatted{
    Error in grid.Call.graphics("L_text", as.graphicsAnnot(x$label), x$x,  )
        Invalid font type
    Calls: print ... drawDetails.text -> grid.Call.graphics -> .Call.graphics
    }
}
\seealso{\code{\link[lattice]{histogram}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

create.histogram(
    # filename = tempfile(pattern = 'Histogram_Simple', fileext = '.tiff'),
    x = rnorm(5000),
    main = 'Simple',
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 50
    );

create.histogram(
    # filename = tempfile(pattern = 'Histogram_Simple_Count', fileext = '.tiff'),
    x = rnorm(5000),
    main = 'Simple Count',
    description = 'Histogram created by BoutrosLab.plotting.general',
    type = 'count',
    resolution = 50
    );

# Minimal Input
create.histogram(
    # filename = tempfile(pattern = 'Histogram_Minimal_Input', fileext = '.tiff'),
    x = microarray[,1],
    main = 'Minimal input',
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 50
    );

# Formula Input - dividing by chromosome
chr.data <- data.frame(
    x = microarray$Chr,
    y = microarray[,1]
    );

create.histogram(
    # filename = tempfile(pattern = 'Histogram_Formula_Input', fileext = '.tiff'),
    x = y ~ x,
    data = chr.data,
    main = 'Formula input',
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Axes and Labels
create.histogram(
    # filename = tempfile(pattern = 'Histogram_Axes_Labels', fileext = '.tiff'),
    x = microarray[,1],
    main = 'Axes & labels',
    # Customizing the axes and labels
    xlab.label = 'Bins',
    ylab.label = 'Counts',
    xlimits = c(0, 16),
    xat = seq(0,15,5),
    # set break points for bins
    breaks = seq(floor(min(microarray[,1])), ceiling(max(microarray[,1])), 1),
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 100
    );

\donttest{
# Colour change
create.histogram(
    # filename = tempfile(pattern = 'Histogram_Colours', fileext = '.tiff'),
    x = microarray[,1],
    main = 'Colours',
    xlab.label = 'Bins',
    ylab.label = 'Counts',
    xlimits = c(0, 16),
    xat = seq(0,15,5),
    breaks = seq(floor(min(microarray[,1])), ceiling(max(microarray[,1])), 1),
    # Colours
    col = 'lightgrey',
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Line type
create.histogram(
    # filename = tempfile(pattern = 'Histogram_Line_Type', fileext = '.tiff'),
    x = microarray[,1],
    main = 'Line type',
    xlab.label = 'Bins',
    ylab.label = 'Counts',
    xlimits = c(0, 16),
    xat = seq(0,15,5),
    breaks = seq(floor(min(microarray[,1])), ceiling(max(microarray[,1])), 1),
    col = 'lightgrey',
    # Changing the line type
    lty = 2,
    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Nature style
create.histogram(
    # filename = tempfile(pattern = 'Histogram_Nature_style', fileext = '.tiff'),
    x = microarray[,1],
    main = 'Nature style',
    xlimits = c(0, 16),
    xat = seq(0,15,5),
    breaks = seq(floor(min(microarray[,1])), ceiling(max(microarray[,1])), 1),
    col = 'lightgrey',

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Histogram created by BoutrosLab.plotting.general',
    resolution = 200
    );
}
}
\keyword{hplot}
