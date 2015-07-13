\name{create.violinplot}
\alias{create.violinplot}
\title{Make a violin plot}
\description{This function takes a dataframe and writes a pretty TIFF violin plot}
\usage{
create.violinplot(formula, data, filename = NULL, main = NULL, 
xlab.label = tail(sub('~','',formula[-2]),1), 
ylab.label = tail(sub('~','',formula[-3]),1), 
xaxis.lab = TRUE, yaxis.lab = TRUE, lwd = 1, xaxis.rot = 0, 
yaxis.rot = 0, ylimits = NULL, yat = TRUE, xaxis.cex = 2, yaxis.cex = 2, 
main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', ylab.col = 'black', 
xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = c(1,0), yaxis.tck = c(1,1), 
fill.colour = "black", border.lwd = 1, extra.points = NULL, extra.points.pch = 21, 
extra.points.col = "white", extra.points.border = "black", extra.points.cex = 1, 
start = NULL, end = NULL, scale = FALSE, horizontal = FALSE, top.padding = 0.1, 
bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.3, width = 6, height = 6, 
resolution = 1600, size.units = 'in', enable.warnings = FALSE, key = NULL, legend = NULL,
description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, 
ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, 
col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', 
yaxis.fontface = 'bold', style = 'BoutrosLab');
}
\arguments{
    \item{formula}{The formula used to extract the violin components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{lwd}{Line width, defaults to 1}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{ylimits}{Two-element vector giving the y-axis limits, default is automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, default is automatic}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 2}
    \item{yaxis.cex}{Size of y-axis tick labels, defaults to 2}
    \item{main.cex}{Size of text for main plot title, defaults to 3}
    \item{xlab.cex}{Size of x-axis label, defaults to 3}
    \item{ylab.cex}{Size of y-axis label, defaults to 3}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to c(1,0)}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to c(1,1)}
    \item{fill.colour}{Colour to use for filling the interior of the violin plots, defaults to \dQuote{black}}
    \item{border.lwd}{Width of the exterior boundary of the violin plots, defaults to 1}
    \item{extra.points}{A list of numeric vectors, each one of length equal to the number of violins to be plotted. Specifies a set or sets of extra points to be plotted along the vertical spine of each violin plot. Defaults to NULL (no points to be added)}
    \item{extra.points.pch}{A vector of the same length as extra.points specifying the symbol to use for each set of points. Defaults to 21}
    \item{extra.points.col}{A vector of the same length as extra.points specifying the colour to use for each set of points. Defaults to \dQuote{white}}
    \item{extra.points.border}{A vector of the same length as extra.points specifying the border colour to use for points >=21. Defaults to \dQuote{black}}
    \item{extra.points.cex}{A vector of the same length as extra.points specifying the size of each set of points. Defaults to 1}
    \item{start}{Start of boundary cutoff, default is NULL for no boundary}
    \item{end}{End of boundary cutoff, default is NULL for no boundary}
    \item{scale}{Logical; Scales the violin plots, see ?panel.violin for more details, default is FALSE}
    \item{horizontal}{Logical; Determines whether to draw violin plot horizontally or vertically; default is FALSE; If \code{horizontal} is \code{FALSE}, x will be coerced to a factor or shingle, and vise versa.}
    \item{top.padding}{A number giving the top padding in multiples of the lattice default}
    \item{bottom.padding}{A number giving the bottom padding in multiples of the lattice default}
    \item{left.padding}{A number giving the left padding in multiples of the lattice default}
    \item{right.padding}{A number giving the right padding in multiples of the lattice default}
    \item{width}{Figure width, defaults to 6 inches}
    \item{height}{Figure height, defaults to 6 inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{size.units}{Figure units, defaults to inches}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{key}{Add a key to the plot. See xyplot.}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{description}{Short description of image/plot; default NULL}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
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
\seealso{\code{\link[lattice]{bwplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x = c(rep(rnorm(50),5)),
    y = as.factor(sample(LETTERS[1:5],250,TRUE))
    );

create.violinplot(
    filename = 'Violinplot_Simple.tiff',
    formula = x ~ y,
    data = simple.data
    );

# load real datasets
violin.data <- data.frame(
    values = c(t(microarray[1:10, 1:58])),
    genes = rep(rownames(microarray)[1:10], each = 58),
    sex = patient$sex
    );

# Minimal input 
create.violinplot(
    filename = 'Violinplot_Minimal_Input.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Minimal input',
    xaxis.rot = 90,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Axes & Labels
create.violinplot(
    filename = 'Violinplot_Axes_Labels.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Axes & labels',
    xaxis.rot = 90,
    # Adjusting axes
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Range
create.violinplot(
    filename = 'Violinplot_Range.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Range',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    # adjusted y-axis limits
    ylimits = c(0, 11),
    yat = seq(0, 10, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # Specify range
    start = 1,
    end = 10,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Scaling
create.violinplot(
    filename = 'Violinplot_Scale.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Scale',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # Scale
    scale = TRUE,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Extra points
median.points <- unlist(tapply(violin.data$values, violin.data$genes, median));
top.points <- unlist(tapply(violin.data$values, violin.data$genes, quantile, 0.90));

create.violinplot(
    filename = 'Violinplot_Points.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Extra points',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # Adding median and 90th percentile
    extra.points = list(median.points, top.points),
    extra.points.pch = 21,
    extra.points.col = c('white','grey'),
    extra.points.cex = 0.5,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Colours
create.violinplot(
    filename = 'Violinplot_Colour.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Colour',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    extra.points = list(median.points, top.points),
    extra.points.pch = 21,
    extra.points.col = c('white','grey'),
    extra.points.cex = 0.5,
    # Colour
    fill.colour = 'dodgerblue',
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Custom labels
create.violinplot(
    filename = 'Violinplot_Custom_Labels.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Custom labels',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 16),
    yat = c(0,1,2,4,8,16),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    extra.points = list(median.points, top.points),
    extra.points.pch = 21,
    extra.points.col = c('white','grey'),
    extra.points.cex = 0.5,
    fill.colour = 'dodgerblue',
    # customizing labels
    yaxis.lab = c(
        0,
        expression(paste('2'^'0')),
        expression(paste('2'^'1')),
        expression(paste('2'^'2')),
        expression(paste('2'^'4')),
        expression(paste('2'^'5'))
        ),
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Orientation
create.violinplot(
    filename = 'Violinplot_Orientation.tiff',
    # switch formula
    formula = genes ~ values,
    data = violin.data,
    main = 'Orientation',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    ylab.label = 'Gene',
    xlab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    extra.points = list(median.points, top.points),
    extra.points.pch = 21,
    extra.points.col = c('white','grey'),
    extra.points.cex = 0.5,
    fill.colour = 'dodgerblue',
    # orientation
    horizontal = TRUE,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# background
create.violinplot(
    filename = 'Violinplot_Background.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Background rectangle',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.label = 'Gene',
    ylab.label = 'Change in expression',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    extra.points = list(median.points, top.points),
    extra.points.pch = 21,
    extra.points.col = c('white','grey'),
    extra.points.cex = 0.5,
    fill.colour = 'dodgerblue',
    # background
    add.rectangle = TRUE,
    xleft.rectangle = seq(0.5, 8.5, 2),
    ybottom.rectangle = 0,
    xright.rectangle = seq(1.5, 9.5, 2),
    ytop.rectangle = 13,
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,
    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

# Nature style
create.violinplot(
    filename = 'Violinplot_Nature_style.tiff',
    formula = values ~ genes,
    data = violin.data,
    main = 'Nature style',
    xaxis.rot = 90,
    xaxis.cex = 1,
    yaxis.cex = 1,
    ylimits = c(0, 13),
    yat = seq(0, 12, 2),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    fill.colour = 'dodgerblue',
    add.rectangle = TRUE,
    xleft.rectangle = seq(0.5, 8.5, 2),
    ybottom.rectangle = 0,
    xright.rectangle = seq(1.5, 9.5, 2),
    ytop.rectangle = 13,
    col.rectangle = 'grey',
    alpha.rectangle = 0.5,

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.lab = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.lab = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Violinplot created by BoutrosLab.plotting.general'
    );

}
\keyword{hplot}
