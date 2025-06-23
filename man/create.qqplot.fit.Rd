\name{create.qqplot.fit}
\alias{create.qqplot.fit}
\title{Make a quantile-quantile plot of a sample}
\description{Takes a sample and creates a qq plot against a theoretical distribution, possibly conditioned on other variables.}
\usage{
create.qqplot.fit(
	x,
	data = NA,
	filename = NULL,
	groups = NULL,
	confidence.bands = FALSE,
	conf = 0.95,
        confidence.method = 'both',
	reference.line.method = 'quartiles',
	distribution = qnorm,
	aspect = 'fill',
        prepanel = NULL,
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
        xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xaxis.col = 'black',
	yaxis.col = 'black',
	xaxis.fontface = 'bold',
        yaxis.fontface = 'bold',
	xaxis.log = FALSE,
	yaxis.log = FALSE,
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.tck = 1,
        yaxis.tck = 1,
	add.grid = FALSE,
	xgrid.at = xat,
	ygrid.at = yat,
	type = 'p',
	cex = 0.75,
	pch = 19,
	col = 'black',
        col.line = 'grey',
	lwd = 2,
	lty = 1,
	axes.lwd = 2.25,
	key = list(text = list(lab = c(''))),
	legend = NULL,
        add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
        ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	top.padding = 3,
	bottom.padding = 0.7,
        left.padding = 0.5,
	right.padding = 0.1,
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
    \item{x}{A formula or a numeric vector}
    \item{data}{An optional data source if x is a formula}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{groups}{The grouping variable in the data-frame}
    \item{confidence.bands}{Add confidence bands or not, default to FALSE. Note that in this function, the confidence band can only be added to a single plot, not for multi-qq plot.}
    \item{conf}{Confidence level, default to 0.95}
    \item{confidence.method}{Methods used to draw confidence bands: \dQuote{simultaneous}, \dQuote{pointwise}, \dQuote{both}, defaults to \dQuote{both}.}
    \item{reference.line.method}{Methods used to draw reference line and must be one of \dQuote{quartiles}(default), \dQuote{diagonal}, \dQuote{robust}. \dQuote{quartiles} will draw a line across 1/4 and 3/4 quantiles, \dQuote{diagonal} will draw a 0-1 line, \dQuote{robust} will draw a best fit line basing on linear model. Note: for multi-panel plot, only the default one is applicable.}
    \item{distribution}{A quantile function that takes a vector of probabilities as argument and produces the corresponding quantiles from a theoretical distribution, defaults to \dQuote{qnorm}, that is normal distribution.}
    \item{aspect}{This argument controls the physical aspect ratio of the panels, defaults to \dQuote{fill}}
    \item{prepanel}{A function that takes the same arguments as the \dQuote{panel}}
    \item{main}{The main plot title}
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of the overall plot title, defaults to 3}
    \item{xlab.label}{x-axis title}
    \item{ylab.label}{y-axis title}
    \item{xlab.cex}{Size of x-axis label, defaults to 2.5}
    \item{ylab.cex}{Size of y-axis label, defaults to 2.5}
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
    \item{xaxis.cex}{Size of x-axis scales, defaults to 1.5}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 1.5}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.log}{Logical indicating whether x-variable should be in logarithmic scale (and what base if numeric)}
    \item{yaxis.log}{Logical indicating whether y-variable should be in logarithmic scale (and what base if numeric)}
    \item{xaxis.rot}{Counterclockwise rotation of text in x-axis scales in degrees, defaults to 0}
    \item{yaxis.rot}{Counterclockwise rotation of text in y-axis scales in degrees, defaults to 0}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{add.grid}{Default manner of drawing grid lines}
    \item{xgrid.at}{Vector listing where the x-axis grid lines should be drawn, defaults to xat}
    \item{ygrid.at}{Vector listing where the y-axis grid lines should be drawn, defaults to yat}
    \item{type}{Plot type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point colour}
    \item{col.line}{QQ line colour, defaults to grey}
    \item{lwd}{Specifies line width, defaults to 2}
    \item{lty}{Specifies line style, defaults to 1 (solid)}
    \item{axes.lwd}{Thickness of width of axes lines}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x ooordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{top.padding}{A number giving the top padding in multiples of the lattice default}
    \item{bottom.padding}{A number giving the bottom padding in multiples of the lattice default}
    \item{left.padding}{A number giving the left padding in multiples of the lattice default}
    \item{right.padding}{A number giving the right padding in multiples of the lattice default}
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
\author{Ying Wu}
\section{Warning}{
Note that the confidence band only works for a single panel qq plot, not for grouped data and multi-qq plot. Why? What's missing?
}
\seealso{\code{\link[lattice]{qqmath}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Simple', fileext = '.tiff'),
    x = rnorm(300),
    # choosing to compare against a uniform distribution
    distribution = qunif,
    resolution = 100
    );

# Minimal Input
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Minimal_Input', fileext = '.tiff'),
    x = microarray[1:500,1],
    # choosing to compare against a uniform distribution
    distribution = qunif,
    main = 'Minimal input',
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Axes and Labels
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Axes_Labels', fileext = '.tiff'),
    x = microarray[1:500,1],
    distribution = qunif,
    main = 'Axes & labels',
    # Adding axes labels
    xlab.label = 'qunif',
    ylab.label = 'sample values',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Confidence bands 
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Confidence_Bands', fileext = '.tiff'),
    x = microarray[1:500,1],
    distribution = qunif,
    main = 'Confidence bands',
    xlab.label = 'qunif',
    ylab.label = 'sample values',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    # Adding confidence bands (auto-generates legend)
    confidence.bands = TRUE,
    confidence.method = 'both',
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

\donttest{
# Multiple qq plot conditioned on a variable
# 'Formula' format of data
chr.locations <- microarray$Chr[1:500];
chr.locations <- replace(chr.locations, which(chr.locations == 1), 'Chromosome 1');
chr.locations <- replace(chr.locations, which(chr.locations == 2), 'Chromosome 2');

qqplot.data <- data.frame(
    value = microarray[1:500,1],
    chr = chr.locations
    );

create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Multiple', fileext = '.tiff'),
    x = ~ value | chr,
    data = qqplot.data,
    distribution = qunif,
    main = 'Multiple plots',
    xlab.label = 'qunif',
    ylab.label = 'sample values',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    confidence.bands = TRUE,
    confidence.method = 'simultaneous',
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Grouped qq plot
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Grouped', fileext = '.tiff'),
    x = ~ value,
    data = qqplot.data,
    # Adding groups
    groups = qqplot.data$chr,
    # Colouring groups
    col = default.colours(2),
    # Setting different plotting characters
    pch = c(15, 19),
    distribution = qunif,
    main = 'Grouped & legend',
    xlab.label = 'qunif',
    ylab.label = 'sample values',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    confidence.bands = TRUE,
    confidence.method = 'simultaneous',
    # Adding legend for groups
    key = list(
        text = list(
            lab = c('1','2'),
            cex = 1,
            col = 'black'
            ),
        points = list(
            pch = c(15, 19),
            col = default.colours(2),
            cex = 1
            ),
        x = 0.04,
        y = 0.95,
        padding.text = 2
        ),
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Correlation Key
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Correlation_Key', fileext = '.tiff'),
    x = ~ value,
    data = qqplot.data,
    groups = qqplot.data$chr,
    col = default.colours(2),
    pch = c(15, 19),
    distribution = qunif,
    main = 'Correlation key',
    xlab.label = 'qunif',
    ylab.label = 'sample values',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    confidence.bands = TRUE,
    confidence.method = 'simultaneous',
    # Adjusting legend to contain multiple keys
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                        text = list(
                            lab = c('1','2'),
                            cex = 1,
                            col = 'black'
                            ),
                        points = list(
                            pch = c(15, 19),
                            col = default.colours(2),
                            cex = 1
                            ),
                        x = 0.14,
                        y = 0.80,
                        padding.text = 2
                    )
                )
            ),
        inside = list(
            fun = draw.key,
            args = list(
                key = get.corr.key(
                    x = runif(500),
                    y = qqplot.data$value,
                    label.items = c('spearman', 'kendall','beta1'),
                    alpha.background = 0,
                    key.cex = 1
                    )
                ),
            x = 0.75,
            y = 0.20,
            corner = c(0,1)
            )
        ),
    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 100
    );

# Nature style
create.qqplot.fit(
    # filename = tempfile(pattern = 'QQfit_Nature_style', fileext = '.tiff'),
    x = microarray[1:500,1],
    distribution = qunif,
    main = 'Nature style',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xaxis.cex = 1,
    yaxis.cex = 1,
    add.grid = TRUE,
    confidence.bands = TRUE,
    confidence.method = 'both',

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'QQplot fit created by BoutrosLab.plotting.general',
    resolution = 1200
    );
}
}
\keyword{hplot}
