\name{create.densityplot}
\alias{create.densityplot}
\title{Make a density plot}
\description{Takes a list of vectors and creates a density-plot with each vector as a separate curve}
\usage{
create.densityplot(
	x,
	filename = NULL,
	main = NULL,
	main.just = 'center',
	main.x = 0.5,
	main.y = 0.5,
	main.cex = 3,
	xlab.label = NULL,
	ylab.label = 'Density',
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
	type = 'l',
	lty = 'solid',
	cex = 0.75,
	pch = 19,
	col = 'black',
	lwd = 2,
	bandwidth = 'nrd0',
	bandwidth.adjust = 1,
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	xaxis.lab = NA,
	yaxis.lab = NA,
	xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.col = 'black',
	yaxis.col = 'black',
	xaxis.fontface = 'bold',
	yaxis.fontface = 'bold',
	xaxis.tck = 1,
	yaxis.tck = 1,
	xgrid.at = xat,
	ygrid.at = yat,
	key = list(text = list(lab = c(''))),
	legend = NULL,
	top.padding = 0.1,
	bottom.padding = 0.7,
	left.padding = 0.5,
	right.padding = 0.1,
	add.axes = FALSE,
	abline.h = NULL,
	abline.v = NULL,
	abline.lty = NULL,
	abline.lwd = NULL,
	abline.col = 'black',
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	add.text = FALSE, 
	text.labels = NULL, 
	text.x = NULL, 
	text.y = NULL, 
	text.anchor = "centre", 
	text.col = "black", 
	text.cex = 1,
        text.fontface = "bold",
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
    \item{x}{A list of vectors, each of which will be plotted as a separate curve in the final plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{main.just}{The justification of the main title for the plot, default is centered}
    \item{main.x}{The x location of the main title, deault is 0.5}
    \item{main.y}{The y location of the main title, default is 0.5}
    \item{main.cex}{Size of text for main plot title, defaults to 2}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis, defaults to \dQuote{Density}}
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
    \item{type}{Plot type}
    \item{lty}{Line type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point/line colour}
    \item{lwd}{Thickness of width of any best-fit lines}
    \item{bandwidth}{Smoothing bandwidth, or character string giving rule to choose bandwidth ('nrd0', 'nrd', 'ucv', 'bcv', 'sj', or 'sj-ste'). Passed to base R function density.}
    \item{bandwidth.adjust}{Adjustment parameter for the bandwidth (bandwidth used is bandwidth*bandwidth.adjust). Makes it easy to specify bandwidth as a proportion of the default.}
    \item{xlimits}{Two-element vector giving the x-axis limits, defaults to automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, defaults to automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, defaults to automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, defaults to automatic}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 1}
    \item{yaxis.cex}{Size of x-axis tick labels, defaults to 1}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{xgrid.at}{Vector listing where the x-axis grid lines should be drawn, defaults to xat}
    \item{ygrid.at}{Vector listing where the y-axis grid lines should be drawn, defaults to yat}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{top.padding}{A number giving the top padding in multiples of the lattice default}
    \item{bottom.padding}{A number giving the bottom padding in multiples of the lattice default}
    \item{left.padding}{A number giving the left padding in multiples of the lattice default}
    \item{right.padding}{A number giving the right padding in multiples of the lattice default}
    \item{add.axes}{Allow axis lines to be turned on or off}
    \item{abline.h}{Specify the superimposed horizontal line(s)}
    \item{abline.v}{Specify the superimposed vertical line(s)}
    \item{abline.lty}{Specify the superimposed line type}
    \item{abline.lwd}{Specify the superimposed line width}
    \item{abline.col}{Specify the superimposed line colour (defaults to black)}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x coordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour to fill the rectangle's area}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{add.text}{Allow additional text to be drawn, default is FALSE}
    \item{text.labels}{Labels for additional text}
    \item{text.x}{The x co-ordinates where additional text should be placed}
    \item{text.y}{The y co-ordinates where additional text should be placed}
    \item{text.anchor}{Part of text that should be anchored to x/y coordinates. Defaults to 'centre'. Use 'left' or 'right' to left or right-align text.}
    \item{text.col}{The colour of additional text}
    \item{text.cex}{The size of additional text}
    \item{text.fontface}{The fontface for additional text}
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
\seealso{\code{\link[lattice]{xyplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x = rnorm(1000),
    y = rnorm(1000, mean = 3, sd = 3)
    );

create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Simple', fileext = '.tiff'),
    x = simple.data,
    main = 'Simple',
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# format data
format.data <- microarray[1:3,1:58];
format.data <- as.data.frame(t(format.data));

# Minimal Input
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Minimal_Input', fileext = '.tiff'),
    x = format.data,
    main = 'Minimal input',
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 50
    );

# Line type
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Line_Type', fileext = '.tiff'),
    x = format.data,
    main = 'Line type',
    # Line type
    lty = c('solid','dashed','dotted'),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 50
    );

# Axes & Labels
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Axes_Labels', fileext = '.tiff'),
    x = format.data,
    main = 'Axes & labels',
    lty = c('solid','dashed','dotted'),
    # Axes & Labels
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    xat = seq(0, 13, 1),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 50
    );

\donttest{
# Colour change & Legend
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Colour_Legend', fileext = '.tiff'),
    x = format.data,
    main = 'Colour & legend',
    lty = c('solid','dashed','dotted'),
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    # Colours
    col = default.colours(3),
    # Legend
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 21,
                        cex = 1.5,
                        fill = default.colours(3)
                        ),
                    text = list(
                        lab = colnames(format.data)
                        ),
                    padding.text = c(0,5,0),
                    cex = 1
                    )
                ),
            x = 0.65,
            y = 0.97,
            draw = FALSE
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 50
    );

# Correlation key
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Correlation_Key', fileext = '.tiff'),
    x = format.data[,1:2],
    main = 'Correlation key',
    lty = c('solid','dotted'),
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    col = default.colours(2),
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(2),
                        pch = 21,
                        cex = 1.5,
                        fill = default.colours(2)
                        ),
                    text = list(
                        lab = colnames(format.data)[1:2]
                        ),
                    padding.text = c(0,5,0),
                    cex = 1
                    )
                ),
            x = 0.65,
            y = 0.97,
            draw = FALSE
            ),
        # Correlation key accepts two vectors
        inside = list(
            fun = draw.key,
            args = list(
                key = get.corr.key(
                    x = as.numeric(format.data[,1]),
                    y = as.numeric(format.data[,2]),
                    label.items = c('pearson','beta1'),
                    alpha.background = 1,
                    key.cex = 1.2
                    )
                ),
            x = 0.65,
            y = 0.85,
            corner = c(0,1)
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Gridlines
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Gridlines_1', fileext = '.tiff'),
    x = format.data,
    main = 'Gridlines',
    lty = c('solid','dashed','dotted'),
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    col = default.colours(3),
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 21,
                        cex = 1.5,
                        fill = default.colours(3)
                        ),
                    text = list(
                        lab = colnames(format.data)
                        ),
                    padding.text = c(0,5,0),
                    cex = 1
                    )
                ),
            x = 0.65,
            y = 0.97,
            draw = FALSE
            )
        ),
    # Grid lines
    type = c('l','g'),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Gridlines
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Gridlines_2', fileext = '.tiff'),
    x = format.data,
    main = 'Gridlines',
    lty = c('solid','dashed','dotted'),
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    col = default.colours(3),
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 21,
                        cex = 1.5,
                        fill = default.colours(3)
                        ),
                    text = list(
                        lab = colnames(format.data)
                        ),
                    padding.text = c(0,5,0),
                    cex = 1
                    )
                ),
            x = 0.65,
            y = 0.97,
            draw = FALSE
            )
        ),
    # Grid lines
    type = c('l','g'),
    xgrid.at = seq(0,14,1),
    ygrid.at = seq(0,2.5,0.25),
    description = 'Barplot created by BoutrosLab.plotting.general',
    resolution = 200
    );

# Nature style
create.densityplot(
    filename = tempfile(pattern = 'Densityplot_Nature_style', fileext = '.tiff'),
    x = format.data,
    main = 'Nature style',
    lty = c('solid','dashed','dotted'),
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    xlab.cex = 1.5,
    col = default.colours(3),
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = default.colours(3),
                        pch = 21,
                        cex = 1.5,
                        fill = default.colours(3)
                        ),
                    text = list(
                        lab = colnames(format.data)
                        ),
                    padding.text = c(0,5,0),
                    cex = 1
                    )
                ),
            x = 0.65,
            y = 0.97,
            draw = FALSE
            )
        ),
    # Grid lines
    style = 'Nature',

    # demonstrating how to italicize character variables
    ylab.lab = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.lab = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),
    resolution = 200
    );
}
}
\keyword{hplot}
