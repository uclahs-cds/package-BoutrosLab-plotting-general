\name{create.manhattanplot}
\alias{create.manhattanplot}
\title{Make a Manhattan plot}
\description{Takes a data.frame and creates a Manhattan plot}
\usage{
create.manhattanplot(
	formula,
	data,
	filename = NULL,
	main = NULL,
	main.just = 'center',
	main.x = 0.5,
	main.y = 0.5,
	main.cex = 3,
	xlab.label = tail(sub('~', '', formula[-2]), 1),
	ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2,
	ylab.cex = 2,
	xlab.col = 'black',
	ylab.col = 'black',
	xlab.top.label = NULL,
	xlab.top.cex = 2,
	xlab.top.col = 'black',
	xlab.top.just = 'center',
	xlab.top.x = 0.5,
	xlab.top.y = 0,
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	xaxis.lab = NA,
	yaxis.lab = NA,
	xaxis.log = FALSE,
	yaxis.log = FALSE,
	xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.fontface = 'plain',
	yaxis.fontface = 'plain',
	xaxis.col = 'black',
	yaxis.col = 'black',
	xaxis.tck = 0,
	yaxis.tck = c(1,1),
	horizontal = FALSE,
	type = 'p',
	cex = 2,
	pch = '.',
	col = 'black',
	lwd = 1,
	lty = 1,
	alpha = 1,
	strip.col = 'white',
	strip.cex = 1,
	axes.lwd = 1,
	axes.lty = 'dashed',
	key = list(text = list(lab = c(''))),
	legend = NULL,
	layout = NULL,
	as.table = FALSE,
	x.spacing = 0,
	y.spacing = 0,
	x.relation = 'same',
	y.relation = 'same',
	top.padding = 0,
	bottom.padding = 0,
	right.padding = 0,
	left.padding = 0,
	key.top = 0,
	key.left.padding = 0,
	ylab.axis.padding = 1,
	axis.key.padding = 1,
	abline.h = NULL,
	abline.col = 'black',
	abline.lwd = 1,
	abline.lty = 1,
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	add.points = FALSE,
	points.x = NULL,
	points.y = NULL,
	points.pch = 19,
	points.col = 'black',
	points.cex = 1,
	add.text = FALSE,
	text.labels = NULL,
	text.x = NULL,
	text.y = NULL,
	text.col = 'black',
	text.cex = 1,
	text.fontface = 'bold',
	height = 6,
	width = 10,
	size.units = 'in',
	resolution = 1600,
	enable.warnings = FALSE,
	style = 'BoutrosLab',
	description = 'Created with BoutrosLab.plotting.general',
	preload.default = 'custom',
        use.legacy.settings = FALSE,
        inside.legend.auto = FALSE,
	...
	);
    }
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of text for main plot title}
    \item{xlab.label}{x-axis label}
    \item{ylab.label}{y-axis label}
    \item{xlab.cex}{Size of x-axis label, defaults to 2}
    \item{ylab.cex}{Size of y-axis label, defaults to 2}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xlab.top.label}{The label for the top x-axis}
    \item{xlab.top.cex}{Size of top x-axis label}
    \item{xlab.top.col}{Colour of the top x-axis label}
    \item{xlab.top.just}{Justification of the top x-axis label, defaults to centered}
    \item{xlab.top.x}{The x location of the top x-axis label}
    \item{xlab.top.y}{The y location of the top y-axis label}
    \item{xlimits}{Two-element vector giving the x-axis limits, defaults to automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, defaults to automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, defaults to automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, defaults to automatic}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.log}{Logical indicating whether x-variable should be in logarithmic scale (and what base if numeric)}
    \item{yaxis.log}{Logical indicating whether y-variable should be in logarithmic scale (and what base if numeric)}
    \item{xaxis.cex}{Size of x-axis scales, defaults to 1}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 1}
    \item{xaxis.rot}{Counterclockwise rotation of text in x-axis scales in degrees, defaults to 0}
    \item{yaxis.rot}{Counterclockwise rotation of text in y-axis scales in degrees, defaults to 0}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick mark, defaults to 1 for both top and bottom axes}
    \item{yaxis.tck}{Specifies the length of the tick mark, defaults to 1 for both top and bottom axes}
    \item{horizontal}{xyplot-specific function that allows you to change if type='h' draws lines to the vertical or horizontal axis}
    \item{type}{Plot type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point/line colour}
    \item{lwd}{Specifies line width, defaults to 1}
    \item{lty}{Specifies line style, defaults to 1 (solid)}
    \item{alpha}{Specifies line transparency, defaults to 1 (opaque)}
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{axes.lwd}{Thickness of width of axis lines}
    \item{axes.lty}{Specifies axis line style, defaults to \dQuote{dashed}}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}  
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}F
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0}
    \item{key.top}{A number specifying the distance at top of key, defaults to 0}
    \item{key.left.padding}{Amount of padding to go onto any legend on the left}
    \item{ylab.axis.padding}{A number specifying the distance of label to the y-axis, defaults to 1}
    \item{axis.key.padding}{A number specifying the distance from the y-axis to the key, defaults to 1}
    \item{abline.h}{Allow horizontal line to be drawn, default to NULL}
    \item{abline.col}{Horizontal line colour, defaults to black}
    \item{abline.lwd}{Specifies horizontal line width, defaults to 1}
    \item{abline.lty}{Specifies horizontal line style, defaults to 1 (solid)}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x coordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{add.points}{Allow additional points to be drawn, default is FALSE}
    \item{points.x}{The x co-ordinates where additional points should be drawn}
    \item{points.y}{The y co-ordinates where additional points should be drawn}
    \item{points.pch}{The plotting character for additional points}
    \item{points.col}{The colour of additional points}
    \item{points.cex}{The size of additional points}
    \item{add.text}{Allow additional text to be drawn, default is FALSE}
    \item{text.labels}{Labels for additional text}
    \item{text.x}{The x co-ordinates where additional text should be placed}
    \item{text.y}{The y co-ordinates where additional text should be placed}
    \item{text.col}{The colour of additional text}
    \item{text.cex}{The size of additional text}
    \item{text.fontface}{The fontface for additional text}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
    \item{description}{Short description of image/plot; default NULL}
    \item{preload.default}{ability to set multiple sets of diffrent defaults depending on publication needs}
    \item{use.legacy.settings}{boolean to set wheter or not to use legacy mode settings (font)}
    \item{inside.legend.auto}{boolean specifying whether or not to use the automatic inside legend function}
    \item{...}{Additional arguments to be passed to xyplot}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\author{Christine P'ng, Cindy Q. Yao}
\section{Warning}{
If this function is called without capturing the return value, or specifying a filename, it may crash while trying to draw the histogram. In particular, if a script that uses such a call of create histogram is called by reading the script in from the command line, it will fail badly, with an error message about unavailable fonts:\cr
        \preformatted{
    Error in grid.Call.graphics("L_text", as.graphicsAnnot(x$label), x$x,  )
        Invalid font type
    Calls: print ... drawDetails.text -> grid.Call.graphics -> .Call.graphics
    }
}
\seealso{\code{\link[lattice]{xyplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);
simple.data <- data.frame(
    x = runif(20000, 0, 1),
    y = 1:20000
    );

create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Simple', fileext = '.tiff'),
    formula = -log10(x) ~ y,
    data = simple.data,
    main = 'Simple',
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 50
    );

# set up chromosome covariate colours to use for chr covariate, below
chr.colours <- force.colour.scheme(microarray$Chr, scheme = 'chromosome');

# make chr covariate and chr labels 
chr.n.genes       <- vector();
chr.tck           <- vector();
chr.pos.genes     <- vector();
chr.break         <- vector();
chr.break[1]      <- 0;
# get a list of chromosomes to loop
chr <- unique(microarray$Chr);

# loop over each chromosome
for ( i in 1:length(chr) ) {

    # get the number of genes that belong to one chromosome
    n <- sum(microarray$Chr == chr[i]);

    # calculate where the labels go
    chr.n.genes[i]   <- n;
    chr.break[i+1] <- n + chr.break[i];
    chr.pos.genes[i] <- floor(chr.n.genes[i]/2);
    chr.tck[i]       <- chr.pos.genes[i] + which(microarray$Chr == chr[i])[1];
    }

# add an indicator function for the data-frame
microarray$ind <- 1:nrow(microarray);

# Minimal input
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Minimal_Input', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Minimal input',
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 100
    );

# Custom Axes
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Custom_Axes', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Custom axes',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression('P'['adjusted']),
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 100
    );

# Colour scheme
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Colour_Scheme', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Colour scheme',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression('P'['adjusted']),
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    col = chr.colours,
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 200
    );

# Plotting Character
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Plotting_Character', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Plotting character',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression('P'['adjusted']),
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    col = chr.colours,
    # Change plotting character and size of plotting character
    pch = 18,
    cex = 0.75,
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 200
    );

\donttest{
# Line
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Added_Line', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Line',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression('P'['adjusted']),
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    col = chr.colours,
    pch = 18,
    cex = 0.75,
    # draw horizontal line
    abline.h = 2,
    abline.lty = 2,
    abline.lwd = 1,
    abline.col = 'black',
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 200
    );

# Background shading
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_BG', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Bg rectangles',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression('P'['adjusted']),
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    col = chr.colours,
    pch = 18,
    cex = 0.75,
    abline.h = 2,
    abline.lty = 2,
    abline.lwd = 1,
    abline.col = 'black',
    # Adding rectangles
    add.rectangle = TRUE,
    xleft.rectangle = chr.break[seq(1, length(chr.break) - 1, 2)],
    ybottom.rectangle = 0,
    xright.rectangle = chr.break[seq(2, length(chr.break) - 1, 2)],
    ytop.rectangle = 4.5,
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,
    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 200
    );

# Nature style
create.manhattanplot(
    filename = tempfile(pattern = 'Manhattan_Nature_style', fileext = '.tiff'),
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Nature style',
    xat = chr.tck,
    xaxis.lab = c(1:22, 'X', 'Y'),
    xaxis.tck = 0,
    xaxis.cex = 1,
    yaxis.cex = 1,
    yat = seq(0,5,1),
    yaxis.lab = c(
        1,
        expression(10^-1),
        expression(10^-2),
        expression(10^-3),
        expression(10^-4)
        ),
    col = chr.colours,
    pch = 18,
    cex = 0.75,
    abline.h = 2,
    abline.lty = 2,
    abline.lwd = 1,
    abline.col = 'black',
    # Adding rectangles
    add.rectangle = TRUE,
    xleft.rectangle = chr.break[seq(1, length(chr.break) - 1, 2)],
    ybottom.rectangle = 0,
    xright.rectangle = chr.break[seq(2, length(chr.break) - 1, 2)],
    ytop.rectangle = 4.5,
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Manhattan plot created using BoutrosLab.plotting.general',
    resolution = 1200
    );
}
}
\keyword{hplot}
