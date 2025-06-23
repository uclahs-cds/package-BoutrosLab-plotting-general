\name{create.segplot}
\alias{create.segplot}
\title{Make a segplot}
\description{Takes a data.frame and creates a segplot}
\usage{
create.segplot(
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
	xaxis.lab = TRUE,
	yaxis.lab = TRUE,
	xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xaxis.col = 'black',
	yaxis.col = 'black',
	xaxis.fontface = 'bold',
	yaxis.fontface = 'bold',
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.tck = 1,
	yaxis.tck = 1,
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	abline.h = NULL,
	abline.v = NULL,
	abline.lty = 1,
	abline.lwd = 1,
	abline.col = 'black',
	segments.col = 'black',
	segments.lwd = 1,
	layout = NULL,
	as.table = FALSE,
	x.spacing = 0,
	y.spacing = 0,
	x.relation = 'same',
	y.relation = 'same',
	top.padding = 0.5,
	bottom.padding = 2,
	right.padding = 1,
	left.padding = 2,
	ylab.axis.padding = 0,
	level = NULL,
	col.regions = NULL,
	centers = NULL,
	plot.horizontal = TRUE,
	draw.bands = FALSE,
	pch = 16,
	symbol.col = 'black',
	symbol.cex = 1,
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	axes.lwd = 1,
	key = NULL,
	legend = NULL,
	height = 6,
	width = 6,
	size.units = 'in',
	resolution = 1600,
	enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab',
	preload.default = 'custom',
	use.legacy.settings = FALSE,
	inside.legend.auto = FALSE,
	disable.factor.sorting = FALSE
	)
}
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of text for main plot title, defaults to 3}
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
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.cex}{Size of x-axis scales, defaults to 1.5}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 1.5}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.fontface}{Fontface for the x-axis scales, defaults to \dQuote{plain}}
    \item{yaxis.fontface}{Fontface for the y-axis scales, defaults to \dQuote{plain}}
    \item{xaxis.rot}{Counterclockwise rotation of text in x-axis scales in degrees, defaults to 0}
    \item{yaxis.rot}{Counterclockwise rotation of text in y-axis scales in degrees, defaults to 0}
    \item{xaxis.tck}{Specifies the length of the tick mark, defaults to 1 for both top and bottom axes}
    \item{yaxis.tck}{Specifies the length of the tick mark, defaults to 1 for both top and bottom axes}
    \item{xlimits}{Two-element vector giving the x-axis limits, defaults to automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, defaults to automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, defaults to automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, defaults to automatic}
    \item{abline.h}{Allow horizontal line to be drawn, default to NULL}
    \item{abline.v}{Allow vertical line to be drawn, default to NULL}
    \item{abline.lty}{Specifies horizontal line style, defaults to 1 (solid)}
    \item{abline.lwd}{Specifies horizontal line width, defaults to 1}
    \item{abline.col}{Horizontal line colour, defaults to black}
    \item{segments.col}{Colour of segments, defaults to \dQuote{black}}
    \item{segments.lwd}{Line width of segments, defaults to 1}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}  
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.1}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0.7}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0.1}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0.5}
    \item{ylab.axis.padding}{A number specifying the distance of ylabel to the y-axis, defaults to 1}, 
    \item{level}{Optional covariate that determines colour coding of the segments, if specified overwrites segments.col, can contain actual colors or values to determine colors, then col.regions should be defined}
    \item{col.regions}{Vector of colors, define if level is numeric}
    \item{centers}{Optional vector for centers of segments, defaults to NULL}  
    \item{plot.horizontal}{Logical whether segments should be drawn horizontally (default) or vertically}
    \item{draw.bands}{Logical to specify whether to draw lines (default) or rectangles}
    \item{pch}{Plotting character for centers}
    \item{symbol.col}{Colour of plotting character for centers, defaults to \dQuote{black}}
    \item{symbol.cex}{Size of plotting character for centers, defaults to 1}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x coordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{axes.lwd}{Specifies axes line width, defaults to 1}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{description}{Short description of image/plot; default NULL}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
    \item{preload.default}{ability to set multiple sets of diffrent defaults depending on publication needs}
    \item{use.legacy.settings}{boolean to set wheter or not to use legacy mode settings (font)}
    \item{inside.legend.auto}{boolean specifying whether or not to use the automatic inside legend function}
    \item{disable.factor.sorting}{Disable barplot auto sorting factors alphabetically/numerically}
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
\seealso{\code{\link[lattice]{levelplot}}, \code{\link[latticeExtra]{segplot}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    min = runif(10,5,15),
    max = runif(10,15,25),
    labels = as.factor(LETTERS[1:10])
    );

create.segplot(
    # filename = tempfile(pattern = 'Segplot_simple', fileext = '.tiff'),
    formula = labels ~ min + max,
    data = simple.data,
    resolution = 50
    );

# load some data
length.of.gene <- apply(microarray[1:10,60:61], 1, diff);
bin.length <- length.of.gene;
bin.length[which(bin.length < 20000)] <- 'A';
bin.length[which(bin.length < 40000)] <- 'B';
bin.length[which(bin.length < 60000)] <- 'C';

segplot.data <- data.frame(
    min = apply(microarray[1:10,1:58], 1, min),
    max = apply(microarray[1:10,1:58], 1, max),
    median = apply(microarray[1:10,1:58], 1, median),
    gene = as.factor(rownames(microarray)[1:10]),
    # approximating length of gene 
    length = as.factor(bin.length)
    );

# Minimal Input using real data
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Minimal_Input', fileext = '.tiff'),
    formula = gene ~ min + max,
    data = segplot.data,
    main = 'Minimal input',
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Axes & Labels
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Axes_Labels', fileext = '.tiff'),
    formula = gene ~ min + max,
    data = segplot.data,
    main = 'Axes & labels',
    # Formatting axes
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

\donttest{
# Bands
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Bands', fileext = '.tiff'),
    formula = gene ~ min + max,
    data = segplot.data,
    main = 'Bands',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    # drawing rectangles instead of lines
    draw.bands = TRUE,
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Colours
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Colours', fileext = '.tiff'),
    formula = reorder(gene, median) ~ min + max,
    data = segplot.data,
    main = 'Colours',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    draw.bands = FALSE,
    # Changing the colours based on a covariate ('level' parameter)
    level = segplot.data$length,
    col.regions = default.colours(3),
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Median
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Median', fileext = '.tiff'),
    formula = gene ~ min + max,
    data = segplot.data,
    main = 'Medians',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    draw.bands = FALSE,
    xat = seq(0, 12, 2),
    level = segplot.data$length,
    col.regions = default.colours(3),
    # Adding center values
    centers = segplot.data$median,
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Reorder by center value
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Reorder', fileext = '.tiff'),
    formula = reorder(gene, median) ~ min + max,
    data = segplot.data,
    main = 'Reordered',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    draw.bands = FALSE,
    centers = segplot.data$median,
    level = segplot.data$length,
    col.regions = default.colours(3),
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Legend
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Legend', fileext = '.tiff'),
    formula = reorder(gene, median) ~ min + max,
    data = segplot.data,
    main = 'Legend',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    draw.bands = FALSE,
    centers = segplot.data$median,
    level = segplot.data$length,
    col.regions = default.colours(3),
    # Adding legend to explain colours
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 19,
                        cex = 1
                        ),
                    text = list(
                        lab = c('1-20000 bp', '20001-40000 bp','40001-60000 bp')
                        ),
                    padding.text = 1,
                    cex = 1
                    )
                ),
            x = 0.60,
            y = 0.15,
            corner = c(0,1)
            )
        ),
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Background
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Background', fileext = '.tiff'),
    formula = reorder(gene, median) ~ min + max,
    data = segplot.data,
    main = 'Background rectangle',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    draw.bands = FALSE,
    centers = segplot.data$median,
    level = segplot.data$length,
    col.regions = default.colours(3),
    # Adding legend to explain colours
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 19,
                        cex = 1
                        ),
                    text = list(
                        lab = c('1-20000 bp', '20001-40000 bp','40001-60000 bp')
                        ),
                    padding.text = 1,
                    cex = 1
                    )
                ),
            x = 0.50,
            y = 0.15,
            corner = c(0,1)
            )
        ),
    # adding background shading
    add.rectangle = TRUE,
    xleft.rectangle = 0,
    ybottom.rectangle = seq(0.5, 8.5, 2),
    xright.rectangle = 13,
    ytop.rectangle = seq(1.5, 9.5, 2),
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,
    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Nature style
create.segplot(
    # filename = tempfile(pattern = 'Segplot_Nature_style', fileext = '.tiff'),
    formula = reorder(gene, median) ~ min + max,
    data = segplot.data,
    main = 'Nature style',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,13),
    xat = seq(0, 12, 2),
    draw.bands = FALSE,
    centers = segplot.data$median,
    level = segplot.data$length,
    col.regions = default.colours(3),
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 19,
                        cex = 1
                        ),
                    text = list(
                        lab = c('1-20000 bp', '20001-40000 bp','40001-60000 bp')
                        ),
                    padding.text = 1,
                    cex = 1
                    )
                ),
            x = 0.50,
            y = 0.15,
            corner = c(0,1)
            )
        ),
    add.rectangle = TRUE,
    xleft.rectangle = 0,
    ybottom.rectangle = seq(0.5, 8.5, 2),
    xright.rectangle = 13,
    ytop.rectangle = seq(1.5, 9.5, 2),
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Segplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# example of bands and lines
create.segplot(
    # filename = tempfile(pattern = 'Segplot_BandsAndLines', fileext = '.tiff'),
    formula = labels ~ min + max,
    data = simple.data,
    draw.bands = c(1,3,5,7,9),
    resolution = 200
    );
}
}
\keyword{hplot}
