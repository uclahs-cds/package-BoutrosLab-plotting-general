\name{create.manhattanplot}
\alias{create.manhattanplot}
\title{Make a Manhattan plot}
\description{Takes a data.frame and creates a Manhattan plot}
\usage{
create.manhattanplot(formula, data, filename = NULL, main = NULL, xlab.label = NULL, 
ylab.label = NULL, main.cex = 2, xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', 
ylab.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, 
xaxis.lab = NA, yaxis.lab = NA, xaxis.log = FALSE, yaxis.log = FALSE, xaxis.cex = 1, 
yaxis.cex = 1, xaxis.rot = 0, yaxis.rot = 0, xaxis.fontface = 'plain', 
yaxis.fontface = 'plain', xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 0, 
yaxis.tck = c(1,1), type = 'p', cex = 2, pch = '.', col = 'black', 
strip.col = "white", strip.cex = 1, lwd = 1, lty = 1, alpha = 1, axis.lwd = 1, 
key = list(text = list(lab = c(''))), legend = NULL, x.spacing = 0, y.spacing = 0, 
top.padding = 0, bottom.padding = 0, right.padding = 0, left.padding = 0, 
key.top = 0, ylab.axis.padding = 1, axis.key.padding = 1, x.relation = "same", 
y.relation = "same", layout = NULL, as.table = FALSE, axes.lty = 'dashed', 
abline.h = NULL, abline.col = "black", abline.lwd = 1, abline.lty = 1, 
add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, 
xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', 
alpha.rectangle = 1, add.points = FALSE, points.x = NULL, points.y = NULL, 
points.pch = 19, points.col = 'black', points.cex = 1, add.text = FALSE, 
text.labels = NULL, text.x = NULL, text.y = NULL, text.col = 'black', 
text.cex = 1, text.fontface = 'bold', key.left.padding = 0, height = 6, 
width = 10, size.units = 'in', resolution = 1600, enable.warnings = FALSE, 
horizontal = FALSE, description = NULL, ...);
    }
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main title for the plot (space is reclaimed if NULL)}
    \item{xlab.label}{x-axis label}
    \item{ylab.label}{y-axis label}
    \item{main.cex}{Size of text for main plot title}
    \item{xlab.cex}{Size of x-axis labels, defaults to 2}
    \item{ylab.cex}{Size of y-axis labels, defaults to 2}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
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
    \item{type}{Plot type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point/line colour}
    \item{strip.col}{Strip background colour, defaults to \dQuote{white}}
    \item{strip.cex}{Strip title character expansion}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0}
    \item{key.top}{A number specifying the distance at top of key, defaults to 0}
    \item{ylab.axis.padding}{A number specifying the distance of label to the y-axis, defaults to 1}
    \item{axis.key.padding}{A number specifying the distance from the y-axis to the key, defaults to 1}
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}  
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}F
    \item{lwd}{Specifies line width, defaults to 1}
    \item{lty}{Specifies line style, defaults to 1 (solid)}
    \item{alpha}{Specifies line transparency, defaults to 1 (opaque)}
    \item{axis.lwd}{Thickness of width of axis lines}
    \item{key.left.padding}{Amount of padding to go onto any legend on the left}
    \item{height}{Figure height, defaults to 6 inches}
    \item{width}{Figure width, defaults to 6 inches}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution in dpi, defaults to 1600}
    \item{axes.lty}{Specifies axis line style, defaults to \dQuote{dashed}}
    \item{abline.h}{Allow horizontal line to be drawn, default to NULL}
    \item{abline.col}{Horizontal line colour, defaults to black}
    \item{abline.lwd}{Specifies horizontal line width, defaults to 1}
    \item{abline.lty}{Specifies horizontal line style, defaults to 1 (solid)}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
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
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{horizontal}{xyplot-specific function that allows you to change if type="h" draws lines to the vertical or horizontal axis}
    \item{description}{Short description of image/plot; default NULL}
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
simple.data <- data.frame(
    x = runif(20000, 0, 1),
    y = 1:20000
    );

create.manhattanplot(
    filename = 'Manhattan_Simple.tiff',
    formula = -log10(x) ~ y,
    data = simple.data,
    main = 'Simple',
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# set up chromosome covariate colours to use for chr covariate, below
chr.colours <- force.colour.scheme(microarray$Chr, scheme = "chromosome");

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
    filename = 'Manhattan_Minimal_Input.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Minimal input',
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# Custom Axes
create.manhattanplot(
    filename = 'Manhattan_Custom_Axes.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Custom axes',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression("P"["adjusted"]),
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
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# Colour scheme
create.manhattanplot(
    filename = 'Manhattan_Colour_Scheme.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Colour scheme',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression("P"["adjusted"]),
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
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# Plotting Character
create.manhattanplot(
    filename = 'Manhattan_Plotting_Character.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Plotting character',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression("P"["adjusted"]),
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
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# Line
create.manhattanplot(
    filename = 'Manhattan_Added_Line.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Line',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression("P"["adjusted"]),
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
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

# Background shading
create.manhattanplot(
    filename = 'Manhattan_BG.tiff',
    formula = -log10(pval) ~ ind,
    data = microarray,
    main = 'Bg rectangles',
    xlab.label = expression('Chromosomes'),
    ylab.label = expression("P"["adjusted"]),
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
    description = 'Manhattan plot created using BoutrosLab.plotting.general'
    );

}
\keyword{hplot}
