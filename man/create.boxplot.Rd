\name{create.boxplot}
\alias{create.boxplot}
\title{Make a boxplot}
\description{Takes a data.frame and creates a boxplot}
\usage{
create.boxplot(formula, data, filename = NULL, main = NULL, add.stripplot = FALSE,
jitter.factor = 1, jitter.amount = NULL, points.pch = 19, points.col = 'darkgrey',
points.cex = 0.5, points.alpha = 1, abline.h = NULL, abline.v = NULL, abline.type = NULL,
abline.lwd = NULL, abline.col = "black", add.rectangle = FALSE, xleft.rectangle = NULL,
ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, 
col.rectangle = 'transparent', alpha.rectangle = 1, fill = 'transparent', 
box.colour = 'black', symbol.cex = 0.8, lwd = 1, outliers = TRUE, xlab.label = NULL, 
ylab.label = NULL, main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', 
ylab.col = 'black', xaxis.rot = 0, yaxis.rot = 0, xaxis.cex = 2, yaxis.cex = 2, 
xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.col = 'black', yaxis.col = 'black', 
xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, x.spacing = 0, y.spacing = 0, 
top.padding = 0.5, bottom.padding = 2, right.padding = 1, left.padding = 2, 
ylab.axis.padding = 0, x.relation = "same", y.relation = "same", xaxis.tck = 1, 
yaxis.tck = 1, strip.col = "white", strip.cex = 1, layout = NULL, as.table = FALSE, 
height = 6, width = 6, size.units = 'in', resolution = 1000, enable.warnings = FALSE, 
key = NULL, legend = NULL, description = NULL, xaxis.fontface = 'bold', 
yaxis.fontface = 'bold', line.func = NULL, line.from = 0, line.to = 0, 
line.col = 'transparent', line.infront = TRUE, sample.order = 'none', 
style = 'BoutrosLab');
}
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main plot title (space is reclaimed if NULL)}
    \item{abline.h}{Specify the horizontal superimpose line}
    \item{abline.v}{Specify the vertical superimpose line}
    \item{abline.type}{Specify the superimpose line type}
    \item{abline.lwd}{Specify the superimpose line width}
    \item{abline.col}{Specify the superimpose line colour (defaults to black)}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{fill}{The colour to fill the interior of the boxplot, defaults to white}
    \item{box.colour}{Colour of the boxplot, defaults to black}
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
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}.  
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1000}
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
    \item{sample.order}{Should the sample be reordered}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
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
    fill = sex.colour,
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
    fill = sex.colour,
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
    fill = sex.colour,
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
    fill = sex.colour,
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

}
\keyword{hplot}
