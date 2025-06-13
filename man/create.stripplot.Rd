\name{create.stripplot}
\alias{create.stripplot}
\title{Make a strip-plot}
\description{Takes a formula and a data.frame and creates a strip-plot}
\usage{
create.stripplot(
	formula,
	data,
	filename = NULL,
	groups = NULL,
	jitter.data = FALSE,
	jitter.factor = 1,
	jitter.amount = NULL,
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
	xaxis.tck = 0,
	yaxis.tck = 1,
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	lwd = 1,
	pch = 19,
	col = 'black',
	col.border = 'black',
	fill = 'transparent',
	colour.alpha = 1,
	cex = 0.75,
	top.padding = 0.1,
	bottom.padding = 0.7,
	right.padding = 0.3,
	left.padding = 0.5,
	ylab.axis.padding = 1,
	layout = NULL,
	as.table = TRUE,
	x.spacing = 0,
	y.spacing = 0,
	add.median = FALSE,
	median.values = NULL,
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	strip.col = 'white',
	strip.cex = 1,
	strip.fontface = 'bold',
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
    \item{groups}{The grouping variable in the data-frame}
    \item{jitter.data}{Allow data to be staggered, default is FALSE}
    \item{jitter.factor}{Numeric value to apply to jitter, default is 1}
    \item{jitter.amount}{Numeric; amount of noise to add, default is NULL}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}s
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of text for main plot title}
    \item{xlab.label}{X-axis label}
    \item{ylab.label}{Y-axis label}
    \item{xlab.cex}{Size of x-axis label, defaults to 3}
    \item{ylab.cex}{Size of y-axis label, defaults to 3}
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
    \item{xaxis.cex}{Size of x-axis scales, defaults to 2}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 2}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 0}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{xlimits}{Two-element vector giving the x-axis limits, default is automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, default is automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, default is automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, default is automatic}
    \item{lwd}{Line width, defaults to 1}
    \item{pch}{The plotting character (defaults to filled circles)}
    \item{col}{Colour of the plotting character (defaults to black)}
    \item{col.border}{Colour of border when pch > 21. Defaults to black}
    \item{fill}{Fill colour of the plotting character if pch set to 21:25 (defaults to transparent)}
    \item{colour.alpha}{Bias to be added to colour selection (defaults to 1)}
    \item{cex}{The size of the plotting character}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.1}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0.7}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0.3}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0.5}
    \item{ylab.axis.padding}{A number specifying the distance of ylabel to the y-axis, defaults to 1}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}.  
    \item{as.table}{Specifies panel drawing order, default is TRUE to draw from top left corner, moving right then down. Set to FALSE to draw panels from bottom left corner, moving right then up}
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{add.median}{TRUE/FALSE indicating whether lines should be drawn at the group medians, default is FALSE}
    \item{median.values}{A vector of values representing the median of each group, default is NULL}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x coordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{strip.fontface}{Strip text fontface, defaults to bold}
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
\seealso{\code{\link[lattice]{stripplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x = c(rep(rnorm(50),5)),
    y = as.factor(sample(LETTERS[1:5],250,TRUE))
    );

create.stripplot(
    filename = tempfile(pattern = 'Stripplot_simple', fileext = '.tiff'),
    formula = x ~ y,
    data = simple.data,
    resolution = 50
    );

# load real datasets
stripplot.data <- data.frame(
    values = c(t(microarray[1:10, 1:58])),
    genes = rep(rownames(microarray)[1:10], each = 58),
    sex = patient$sex,
    stringsAsFactors = TRUE
    );

# Minimal Input using real data
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_Minimal_Input', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'Minimal input',
    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 50
    );

# Axes & Labels
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_Axes_Labels', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'Axes & labels',
    # formatting axes
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Colour & Legend
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_Colour_Legend', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'Colour & legend',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Colour & points adjustment
    groups = stripplot.data$sex,
    col = c('pink', 'skyblue'),
    pch = 19,
    colour.alpha = 0.5,
    cex = 1,
    # Legend
    key = list(
        space = 'right',
        text = list(
            lab = levels(stripplot.data$sex),
            cex = 1,
            col = 'black'
            ),
        points = list(
            pch = 19,
            col = c('pink','skyblue'),
            alpha = 0.5,
            cex = 1
            ),
        padding.text = 3
        ),
    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 100
    );

\donttest{
# Jitter
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_Jitter', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'Low Jitter',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    groups = stripplot.data$sex,
    col = c('pink', 'skyblue'),
    pch = 19,
    colour.alpha = 0.5,
    cex = 1,
    key = list(
        space = 'right',
        text = list(
            lab = levels(stripplot.data$sex),
            cex = 1,
            col = 'black'
            ),
        points = list(
            pch = 19,
            col = c('pink','skyblue'),
            alpha = 0.4,
            cex = 1
            ),
        padding.text = 3
        ),
    # Custom jitter
    jitter.data = TRUE,
    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Jitter
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_High_Jitter', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'High Jitter',
    xlab.label = 'Change in gene expression',
    ylab.label = 'Gene',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    groups = stripplot.data$sex,
    col = c('pink', 'skyblue'),
    pch = 19,
    colour.alpha = 0.5,
    cex = 1,
    key = list(
        space = 'right',
        text = list(
            lab = levels(stripplot.data$sex),
            cex = 1,
            col = 'black'
            ),
        points = list(
            pch = 19,
            col = c('pink','skyblue'),
            alpha = 0.4,
            cex = 1
            ),
        padding.text = 3
        ),
    # Custom jitter
    jitter.data = TRUE,
    jitter.factor = 0.5,
    jitter.amount = 0.33,
    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Nature style
create.stripplot(
    filename = tempfile(pattern = 'Stripplot_Nature_style', fileext = '.tiff'),
    formula = genes ~ values,
    data = stripplot.data,
    main = 'Nature style',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    groups = stripplot.data$sex,
    col = c('pink', 'skyblue'),
    pch = 19,
    colour.alpha = 0.5,
    cex = 1,
    key = list(
        space = 'right',
        text = list(
            lab = levels(stripplot.data$sex),
            cex = 1,
            col = 'black'
            ),
        points = list(
            pch = 19,
            col = c('pink','skyblue'),
            alpha = 0.4,
            cex = 1
            ),
        padding.text = 3
        ),
    jitter.data = TRUE,
    jitter.factor = 0.5,
    jitter.amount = 0.33,

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Stripplot created by BoutrosLab.plotting.general',
    resolution = 200
    );
}
}
\keyword{hplot}
