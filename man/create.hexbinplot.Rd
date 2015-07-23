\name{create.hexbinplot}
\alias{create.hexbinplot}
\title{Make a hexagonally binned plot}
\description{Takes a data.frame and writes a hexagonally binned plot}
\usage{
create.hexbinplot(formula, data, filename = NULL, main = NULL, aspect = 'xy', 
trans = NULL, inv = NULL, colour.scheme = NULL, colourkey = TRUE, 
colourcut = seq(0, 1, length = 11), mincnt = 1, maxcnt = NULL, main.cex = 2.5, 
xlab.cex = 2.5, ylab.cex = 2.5, xlab.label = tail(sub('~','',formula[-2]),1), 
ylab.label = tail(sub('~','',formula[-3]),1), xlab.col = 'black',
ylab.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, 
xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 2, yaxis.cex = 2, xaxis.rot = 0, 
yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1, 
add.grid = FALSE, abline.h = NULL, abline.v = NULL, abline.type = NULL, 
abline.lwd = NULL, abline.col = 'black', abline.front = FALSE, add.xyline = FALSE, 
xyline.col = "black", xyline.lwd = 1, xyline.lty = 1, add.curves = FALSE, 
curves.exprs = NULL, curves.from = min(data, na.rm = TRUE), 
curves.to = max(data, na.rm = TRUE), curves.col = "black", curves.lwd = 2, 
curves.lty = 1, add.axes = FALSE, 
xbins = 30, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5, 
right.padding = 0.1, height = 6, width = 6, size.units = 'in', resolution = 1600, 
enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL, 
add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, 
xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', 
alpha.rectangle = 1, background.col= 'transparent', xaxis.fontface = 'bold', 
yaxis.fontface = 'bold', style = 'BoutrosLab');
}
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL (default value) returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{aspect}{This argument controls the physical aspect ratio of the panels, defaults to \dQuote{xy}} 
    \item{trans}{function specifying a transformation for the counts such as log, defaults to NULL}
    \item{inv}{the inverse transformation of trans, defaults to NULL}
    \item{colour.scheme}{colour scheme to be used, default NULL gives LinGray colour scale}  
    \item{colourkey}{logical whether a legend should be drawn, defaults to TRUE}
    \item{colourcut}{Vector of values covering [0, 1] that determine hexagon colour class boundaries and hexagon legend size boundaries. Alternatively, an integer (<= maxcnt) specifying the number of equispaced colourcut values in [0,1].}
    \item{mincnt}{Cells with fewer counts are ignored}
    \item{maxcnt}{Cells with more counts are ignored, defaults to auto-generation}
    \item{main.cex}{Size of the main plot title}
    \item{xlab.cex}{Size of x-axis label}
    \item{ylab.cex}{Size of y-axis label}
    \item{xlab.label}{X-axis label}
    \item{ylab.label}{Y-axis label}
    \item{xlimits}{Two-element vector giving the x-axis limits}
    \item{ylimits}{Two-element vector giving the y-axis limits}
    \item{xat}{Vector listing where the x-axis labels should be drawn}
    \item{yat}{Vector listing where the y-axis labels should be drawn}
    \item{xaxis.cex}{Size of x-axis scales, defaults to 2}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 2}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{add.grid}{Allows grid lines to be turned on or off}
    \item{abline.h}{Specify the superimposed horizontal line(s)}
    \item{abline.v}{Specify the superimposed vertical line(s)}
    \item{abline.type}{Specify the superimposed line type}
    \item{abline.lwd}{Specify the superimposed line width}
    \item{abline.col}{Specify the superimposed line colour (defaults to black)}
    \item{abline.front}{If an abline and/or a grid has been added, this controls whether they are drawn in front of the hexbins}
    \item{add.xyline}{Allow y=x line to be drawn, default is FALSE}
    \item{xyline.col}{y=x line colour, defaults to black}
    \item{xyline.lwd}{Specifies y=x line width, defaults to 1}
    \item{xyline.lty}{Specifies y=x line style, defaults to 1 (solid)}
    \item{add.curves}{Allow curves to drawn, default is FALSE}
    \item{curves.exprs}{A list of functions, expressions, or calls using \dQuote{x} as a variable that specify the curves to be drawn}
    \item{curves.from}{Specifies the x co-ordinates at which the start of each curve should be drawn, defaults to drawing the curves to the left edge of the plotting region}
    \item{curves.to}{Specifies the x co-ordinates at which the end of each curve should be drawn, defaults to drawing the curves to the right edge of the plotting region}
    \item{curves.col}{Specifies colours of curves, default is black for each curve}
    \item{curves.lwd}{Specifies width of curves, default is 1 for each curve}
    \item{curves.lty}{Specifies type of curves, default is 1 (solid) for each curve}
    \item{add.axes}{Allow axis lines to be turned on or off}
    \item{xbins}{Number of bins to use in x, defaults to 30}
    \item{top.padding}{A number giving the top padding in multiples of the lattice default}
    \item{bottom.padding}{A number giving the bottom padding in multiples of the lattice default}
    \item{left.padding}{A number giving the left padding in multiples of the lattice default}
    \item{right.padding}{A number giving the right padding in multiples of the lattice default}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{key}{Add a key to the plot. See xyplot.}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{description}{Short description of image/plot; default NULL.}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{background.col}{Specifies the colour for the background of the plot}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\details{\bold{WARNING:} this function uses highly unusual semantics, different from the rest of the \code{BoutrosLab.plotting.general} library.  The underlying hexbinplot function uses an argument called maxcnt to specify the maximum number of counts per cell.  The default behaviour is not sensibly encoded via a \code{NULL} or an \code{NA}, but instead by using the \code{\link{missing}} function.  As a result, we need to use \code{\link{do.call}} semantics to handle this function. This \bold{can mess up anything using \code{\link{substitute}} }including things that generate p-values!}
\author{Maud HW Starmans}
\section{Warning}{
If this function is called without capturing the return value, or specifying a filename, it may crash while trying to draw the histogram. In particular, if a script that uses such a call of create histogram is called by reading the script in from the command line, it will fail badly, with an error message about unavailable fonts:\cr
        \preformatted{
    Error in grid.Call.graphics("L_text", as.graphicsAnnot(x$label), x$x,  )
        Invalid font type
    Calls: print ... drawDetails.text -> grid.Call.graphics -> .Call.graphics
If 'maxcnt' is passed, make sure it is not smaller than the actual maximum count (value depends on nbins). Otherwise, some data may be lost. If you aren't sure what the actual max count is, run this function without specifying the 'maxcnt' parameter using the desired number of bins. 
    }
}
\seealso{\code{\link[lattice]{xyplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x <- rnorm(10000),
    y <- rnorm(10000)
    );

create.hexbinplot(
    filename = 'Hexbinplot_Simple.tiff',
    formula = y ~ x,
    data = simple.data,
    main = 'Simple',
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Set up data
hexbin.data <- data.frame(
    x = microarray[,1],
    y = microarray[,2]
    );

# Minimal Input
create.hexbinplot(
    filename = 'Hexbinplot_Minimal_Input.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Minimal input',
    # formatting bins
    colourcut = seq(0, 1, length = 11),
    # this sets the maximum value plotted -- values greater than this will not appear
    maxcnt = 50,
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Axes & Labels
create.hexbinplot(
    filename = 'Hexbinplot_Axes_Labels.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Axes & labels',
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Customize Axes and labels
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Aspect Ratio
create.hexbinplot(
    filename = 'Hexbinplot_Aspect_Ratio.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Aspect ratio',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Set the aspect ratio to control plot dimensions
    aspect = 2,
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Colour scheme
create.hexbinplot(
    filename = 'Hexbinplot_Colour_Change.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Colour change',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    aspect = 1,
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Specify colour scheme
    colour.scheme = colorRampPalette(c("dodgerblue","paleturquoise","chartreuse","yellow",
        "orange","red")),
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Bin sizes
create.hexbinplot(
    filename = 'Hexbinplot_Bin_Sizes.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Bin sizes',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    aspect = 1,
    colour.scheme = colorRampPalette(c("dodgerblue","paleturquoise","chartreuse", "yellow",
        "orange","red")),
    # Specify bin sizes
    colourcut = seq(0,1,length = 6),
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Correlation Key
create.hexbinplot(
    filename = 'Hexbinplot_Correlation.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Correlation',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    aspect = 1,
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Correlation Key
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = get.corr.key(
                    x = hexbin.data$x,
                    y = hexbin.data$y,
                    label.items = c('beta1', 'spearman'),
                    alpha.background = 0
                    )
                ),
            x = 0.05,
            y = 0.95,
            corner = c(0,1),
            draw = FALSE
            )
        ),
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Grid lines and diagonal
create.hexbinplot(
    filename = 'Hexbinplot_Gridlines.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Gridlines',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    aspect = 1,
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Grid & diagonal
    add.grid = TRUE,
    add.xyline = TRUE,
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Large range
# Generate some fake data with both very low and very high values
set.seed(12345);

x <- c(rnorm(100000,0,0.1),rnorm(1000,0,0.5),rnorm(1000,0,sd=0.75));
y <- c(rnorm(100000,0,0.1),rnorm(1000,0,0.5),rnorm(1000,0,sd=0.75));

fake.data <- data.frame(
    x = x,
    y = y,
    z = y + x*(x+1)/4
    );

create.hexbinplot(
    filename = 'Hexbinplot_Range.tiff',
    formula = z ~ x,
    data = fake.data,
    main = 'Range',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    aspect = 1,
    # Use colourcut to divide the bins appropriately
    colourcut = c(0,0.0002,0.0004,0.0008,0.0016,0.0032,0.0064,0.0128,0.0256,0.0512,0.1024,0.2048,
        0.4096,0.8192,1),
    # Change the colour scheme
    colour.scheme = function(n){BTC(n, beg=1, end=256)},
    background.col = 'grey',
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Outliers
# Generate data with upper bound outlier
set.seed(12345);

x <- c(rnorm(1000,0,0),rnorm(4000,0,0.5));
y <- c(rnorm(1000,0,0),rnorm(4000,0,0.5));

fake.data.outlier <- data.frame(
    x = x,
    y = y,
    z = y + x*(x+1)/4
    );

create.hexbinplot(
    filename = 'Hexbinplot_Outlier.tiff',
    formula = z ~ x,
    data = fake.data.outlier,
    main = 'Outlier',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    aspect = 1,
    # Use colourcut to divide the bins appropriately
    colourcut = c(seq(0,0.01, length = 4),seq(0.0125,0.1,length=4), seq(0.125,1,length=4)),
    xbins = 15,
    mincnt = 0,
    # Change the colour scheme
    colour.scheme = function(n){BTC(n, beg=1, end=256)},
    background.col = 'grey',
    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

# Nature style
create.hexbinplot(
    filename = 'Hexbinplot_Nature_style.tiff',
    formula = y ~ x,
    data = hexbin.data,
    main = 'Nature style',
    xaxis.cex = 1,
    yaxis.cex = 1,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xlimits = c(0,16),
    ylimits = c(0,16),
    xat = seq(0,16,2),
    yat = seq(0,16,2),
    aspect = 1,
    colourcut = seq(0, 1, length = 11),
    maxcnt = 50,
    # Grid & diagonal
    add.grid = TRUE,
    add.xyline = TRUE,

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.lab = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.lab = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'Hexbinplot created by BoutrosLab.plotting.general'
    );

}
\keyword{hplot}
