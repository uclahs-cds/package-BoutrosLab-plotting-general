\name{create.boxplot}
\alias{create.boxplot}
\title{Make a boxplot}
\description{Takes a data.frame and creates a boxplot}
\usage{
create.boxplot(
	formula,
	data,
	filename = NULL,
	main = NULL,
	main.just = "center",
	main.x = 0.5,
	main.y = 0.5,
	add.stripplot = FALSE,
	jitter.factor = 1,
	jitter.amount = NULL,
	points.pch = 19,
	points.col = 'darkgrey',
	points.cex = 0.5,
	points.alpha = 1,
	abline.h = NULL,
	abline.v = NULL,
	abline.type = NULL,
	abline.lwd = NULL,
	abline.col = "black",
	add.rectangle = FALSE,
	xleft.rectangle = NULL,
	ybottom.rectangle = NULL,
	xright.rectangle = NULL,
	ytop.rectangle = NULL,
	col.rectangle = 'transparent',
	alpha.rectangle = 1,
	col = 'transparent',
	border.col = 'black',
	symbol.cex = 0.8,
	lwd = 1,
	outliers = TRUE,
	xlab.label = tail(sub('~','',formula[-2]),1),
	ylab.label = tail(sub('~','',formula[-3]),1),
	main.cex = 3,
	xlab.cex = 2,
	ylab.cex = 2,
	xlab.col = 'black',
	ylab.col = 'black',
	xlab.top.label = NULL,
	xlab.top.cex = 2,
	xlab.top.col = 'black',
	xlab.top.just = "center",
	xlab.top.x = 0.5,
	xlab.top.y = 0,
	xaxis.rot = 0,
	yaxis.rot = 0,
	xaxis.cex = 1.5,
	yaxis.cex = 1.5,
	xaxis.lab = TRUE,
	yaxis.lab = TRUE,
	xaxis.col = 'black',
	yaxis.col = 'black',
	xlimits = NULL,
	ylimits = NULL,
	xat = TRUE,
	yat = TRUE,
	x.spacing = 0,
	y.spacing = 0,
	top.padding = 0.5,
	bottom.padding = 2,
	right.padding = 1,
	left.padding = 2,
	ylab.axis.padding = 0,
	x.relation = "same",
	y.relation = "same",
	xaxis.tck = 1,
	yaxis.tck = 1,
	add.text = FALSE,
	text.labels = NULL,
	text.x = NULL,
	text.y = NULL,
	text.col = 'black',
	text.cex = 1,
	text.fontface = 'bold',
	strip.col = "white",
	strip.cex = 1,
	strip.fontface = 'bold',
	layout = NULL,
	as.table = FALSE,
	height = 6,
	width = 6,
	size.units = 'in',
	resolution = 1600,
	enable.warnings = FALSE,
	key = NULL,
	legend = NULL,
	description = 'Created with BoutrosLab.plotting.general',
	xaxis.fontface = 'bold',
	yaxis.fontface = 'bold',
	line.func = NULL,
	line.from = 0,
	line.to = 0,
	line.col = 'transparent',
	line.infront = TRUE,
	sample.order = 'none',
	order.by = 'median',
	style = 'BoutrosLab',
	add.pvalues = FALSE,
	pvalues.cex = c(1),
	preload.default = 'custom',
	box.ratio = 1
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
    \item{abline.h}{Specify the horizontal superimpose line}
    \item{abline.v}{Specify the vertical superimpose line}
    \item{abline.type}{Specify the superimpose line type}
    \item{abline.lwd}{Specify the superimpose line width}
    \item{abline.col}{Specify the superimpose line colour (defaults to black)}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left x ooordinate of the rectangle to be drawn}
    \item{ybottom.rectangle}{Specifies the bottom y coordinate of the rectangle to be drawn}
    \item{xright.rectangle}{Specifies the right x coordinate of the rectangle to be drawn}
    \item{ytop.rectangle}{Specifies the top y coordinate of the rectangle to be drawn}
    \item{col.rectangle}{Specifies the colour of the rectangle to be drawn}
    \item{alpha.rectangle}{Specifies the colour bias of the rectangle to be drawn}
    \item{col}{The colour to fill the interior of the boxplot, defaults to white}
    \item{border.col}{Colour of the boxplot, defaults to black}
    \item{symbol.cex}{Size of the boxplot outlier-symbol}
    \item{lwd}{Line width, defaults to 1}
    \item{outliers}{logical whether to plot outliers, defaults to TRUE}
    \item{add.stripplot}{logical whether to plot all points, defaults to FALSE}
    \item{jitter.factor}{Numeric value to apply to jitter, default is 1}
    \item{jitter.amount}{Numeric; amount of noise to add, default is NULL}
    \item{points.pch}{pch value to use for stripplot}
    \item{points.col}{colour to use for stripplot}
    \item{points.cex}{cex value to use for stripplot}
    \item{points.alpha}{alpha value to use for stripplot}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis}
    \item{main.cex}{Size of text for main plot title, defaults to 3}
    \item{xlab.cex}{Size of x-axis label, defaults to 3}
    \item{ylab.cex}{Size of y-axis label, defaults to 3}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xlab.top.label}{The label for the top x-axis}
    \item{xlab.top.cex}{Size of top x-axis label}
    \item{xlab.top.col}{Colour of the top x-axis label}
    \item{xlab.top.just}{Justification of the top x-axis label, defaults to centered}
    \item{xlab.top.x}{The x location of the top x-axis label}
    \item{xlab.top.y}{The y location of the top y-axis label}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 2}
    \item{yaxis.cex}{Size of y-axis tick labels, defaults to 2}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xlimits}{Two-element vector giving the x-axis limits}
    \item{ylimits}{Two-element vector giving the y-axis limits}
    \item{xat}{Vector listing where the x-axis labels should be drawn}
    \item{yat}{Vector listing where the y-axis labels should be drawn}
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.5}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 2}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 1}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 2}
    \item{ylab.axis.padding}{A number specifying the distance of y-axis label to the y-axis, defaults to 0},
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{add.text}{Allow additional text to be drawn, default is FALSE}
    \item{text.labels}{Labels for additional text}
    \item{text.x}{The x co-ordinates where additional text should be placed}
    \item{text.y}{The y co-ordinates where additional text should be placed}
    \item{text.col}{The colour of additional text}
    \item{text.cex}{The size of additional text}
    \item{text.fontface}{The fontface for additional text}
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{strip.fontface}{Strip title fontface, defaults to bold}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}.
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{key}{Add a key to the plot. See xyplot.}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{description}{Description of image/plot; default NULL.}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{line.func}{Function for the line that should be drawn on top of plot}
    \item{line.from}{The starting point of the line on the plot}
    \item{line.to}{The ending point of the line on the plot}
    \item{line.col}{Colour of the line on the plot}
    \item{line.infront}{Should the line appear in front of the plot or not}
    \item{sample.order}{String specifying how samples should be ordered. Either none, increasing, or decreasing.}
    \item{order.by}{A string specifying what the sample order should be ordered by, either max, min, median or mean}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
    \item{add.pvalues}{logical determining if pvalues should appear above boxes}
    \item{pvalues.cex}{size of pvalues text},
    \item{preload.default}{ability to set multiple sets of diffrent defaults depending on publication needs}
    \item{box.ratio}{ability to change the box width, defaults to 1}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\author{Maud H.W. Starmans}
\section{Warning}{
If this function is called without capturing the return value, or specifying a filename, it may crash while trying to draw the histogram. In particular, if a script that uses such a call of create histogram is called by reading the script in from the command line, it will fail badly, with an error message about unavailable fonts:\cr
        \preformatted{
    Error in grid.Call.graphics("L_text", as.graphicsAnnot(x$label), x$x,  )
        Invalid font type
    Calls: print ... drawDetails.text -> grid.Call.graphics -> .Call.graphics
    }
}
\seealso{\code{\link[lattice]{bwplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x <- rnorm(1000),
    y <- rep('A',1000)
    );

create.boxplot(
    filename = 'Boxplot_Simple.tiff',
    formula = y ~ x,
    data = simple.data,
    main = 'Simple',
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# add stripplot behind boxplot
create.boxplot(
    filename = 'Boxplot_with_Stripplot.tiff',
    formula = y ~ x,
    data = simple.data,
    main = 'With Stripplot',
    add.stripplot = TRUE,
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# format data
reformatted.data <- data.frame(
    x <- as.vector(t(microarray[1:10,1:58])),
    y <- as.factor(rep(rownames(microarray[1:10,1:58]),each = 58))
    );

# Minimal Input
create.boxplot(
    filename = 'Boxplot_Minimal_Input.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Minimal input',
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Axes and labels
create.boxplot(
    filename = 'Boxplot_Axes_Labels.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Axes & labels',
    # Adjusting axes size
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # Adding y-axis label
    ylab.label = 'Gene',
    # setting axes limits
    xlimits = c(0,13),
    xat = seq(0,12,2),
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Sorting
create.boxplot(
    filename = 'Boxplot_Sorted.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Sorting',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Reordered by median
    sample.order = 'increasing',
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Colour change
sex.colour <- recode.vector(
    patient$sex,
    list(
        dodgerblue = 'male',
        pink = 'female'
        )
    );

create.boxplot(
    filename = 'Boxplot_Colour_Change.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Colour change',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Colour change
    col = sex.colour,
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Legend
create.boxplot(
    filename = 'Boxplot_Legend.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Legend',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    col = sex.colour,
    # legend
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 1.5,
                        fill = c('dodgerblue','pink')
                        ),
                    text = list(
                        lab = c('male','female')
                        ),
                    cex = 1
                    )
                ),
            x = 0.03,
            y = 0.97,
            corner = c(0,1),
            draw = FALSE
            )
        ),
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Orientation
create.boxplot(
    filename = 'Boxplot_Orientation.tiff',
    # switch the order
    formula = x ~ y,
    data = reformatted.data,
    main = 'Orientation',
    xaxis.cex = 1,
    yaxis.cex = 1,
    # adjust the axes
    ylimits = c(0,13),
    yat = seq(0,12,2),
    # rotate the labels
    xaxis.rot = 90,
    xlab.label = 'Gene',
    xlab.cex = 1.5,
    col = sex.colour,
    # legend
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 1.5,
                        fill = c('dodgerblue','pink')
                        ),
                    text = list(
                        lab = c('male','female')
                        ),
                    cex = 1
                    )
                ),
            x = 0.23,
            y = 0.97,
            corner = c(0,1),
            draw = FALSE
            )
        ),
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Background rectangle
create.boxplot(
    filename = 'Boxplot_BG_Rect.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Bg rectangle',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # draw rectangle
    add.rectangle = TRUE,
    xleft.rectangle = 0,
    xright.rectangle = 13,
    ybottom.rectangle = seq(0.5, 8.5, 2),
    ytop.rectangle = seq(1.5, 9.5, 2),
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,
    col = sex.colour,
    # legend
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 1.5,
                        fill = c('dodgerblue','pink')
                        ),
                    text = list(
                        lab = c('male','female')
                        ),
                    cex = 1
                    )
                ),
            x = 0.03,
            y = 0.97,
            corner = c(0,1),
            draw = FALSE
            )
        ),
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Line
create.boxplot(
    filename = 'Boxplot_Line.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Line',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # draw line
    line.func = function(x){c(0.5, 10.5)},
    line.from = 11,
    line.to = 11,
    line.col = 'grey',
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Panel Organization
create.boxplot(
    filename = 'Boxplot_Panels.tiff',
    formula = ~ x | y,
    data = reformatted.data,
    main = 'Panels',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Setting up the layout
    layout = c(2,5),
    x.relation = 'free',
    x.spacing = 1,
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

# Nature format
create.boxplot(
    filename = 'Boxplot_Nature_style.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Nature style',
    xaxis.cex = 1,
    yaxis.cex = 1,

    # set style to Nature
    style = 'Nature',

    # demonstrating how to italicize character variables
    ylab.lab = expression(paste('italicized ', italic('a'))),

    # demonstrating how to create en-dashes
    xlab.lab = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3))
    );

# Sorting by mean and multiple filenames
create.boxplot(
    filename = c('Boxplot_Sorted1.tiff','Boxplot_Sorted2.tiff'),
    formula = y ~ x,
    data = reformatted.data,
    main = 'Sorting',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Reordered by median
    sample.order = 'increasing',
    order.by = 'mean',
    description = 'Boxplot created by BoutrosLab.plotting.general'
    );

create.boxplot(
    filename = 'Boxplot_pvals.tiff',
    formula = y ~ x,
    data = reformatted.data,
    main = 'Sorting',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    ylab.label = 'Gene',
    xlimits = c(0,13),
    xat = seq(0,12,2),
    # Reordered by median
    sample.order = 'increasing',
    order.by = 'mean',
    description = 'Boxplot created by BoutrosLab.plotting.general',
    add.pvalues = TRUE,
    pvalues.cex = 0.5
    );

# Adding text to plot
# Generate normally distributed variables with two different means
set.seed(779);
groupA <- rnorm(n = 100, mean = 10, sd = 2);
groupB <- rnorm(n = 134, mean = 10.5, sd = 2);

# Create data frame for plotting
to.plot <- data.frame(
	y = rep(
		c('1', '2'),
		times = c(100, 134)
		),
	x = c(groupA, groupB)
	);

# Get difference between means
diff.mean <- round(mean(groupB) - mean(groupA), 2);

# Plot and display difference
create.boxplot(
	formula = x ~ y,
	filename = 'boxplot_with_text.tiff',
	data = to.plot,
	add.stripplot = TRUE,
	add.text = TRUE,
	text.labels = bquote(mu[B] - mu[A] == .(diff.mean)),
	text.x = 2.1,
	text.y = 15.3,
	text.col = 'black',
	text.cex = 1.5,
	text.fontface = 'bold',
	resolution = 500,
	ylimits = c(
		min(to.plot$x) - abs(min(to.plot$x) * 0.1),
		max(to.plot$x) + abs(max(to.plot$x) * 0.1)
		)
    );

}
\keyword{hplot}
