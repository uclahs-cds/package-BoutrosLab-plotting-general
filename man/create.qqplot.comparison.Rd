\name{create.qqplot.comparison}
\alias{create.qqplot.comparison}
\title{Make a quantile-quantile plot of two samples}
\description{Takes two samples and creates a qq plot for comparing two distributions, possibly conditioned on other variables}
\usage{
create.qqplot.comparison(x, data = NULL, filename = NULL, aspect = 'fill',
prepanel = NULL, grid = FALSE, groups = NULL, main = NULL, xlab.label = NULL, 
ylab.label = NULL, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, 
xgrid.at = xat, ygrid.at = yat, xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 1.5, 
yaxis.cex = 1.5, main.cex = 3, xlab.cex = 2.5, xaxis.fontface = 'bold', 
xlab.col = 'black', yaxis.fontface = 'bold', ylab.cex = 2.5, ylab.col = 'black', 
xaxis.log = FALSE, yaxis.log = FALSE, xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', 
yaxis.col = 'black', type = 'p', cex = 0.75, pch = 19, col = 'black', lwd = 1, lty = 1, 
axis.lwd = 2.25, xaxis.tck = 1, yaxis.tck = 1, key = list(text = list(lab = c(''))), 
legend = NULL, top.padding = 3, bottom.padding = 0.7, left.padding = 0.5, 
right.padding = 0.1, height = 6, width = 6, size.units = 'in', 
resolution = 1600, enable.warnings = FALSE, description = NULL,add.rectangle = FALSE, 
xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, 
ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1,
style = 'BoutrosLab');
}
\arguments{
    \item{x}{A formula or a list of two numeric vectors}
    \item{data}{An optional data source if x is a formula}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{aspect}{This argument controls the physical aspect ratio of the panels, defaults to \dQuote{fill}}
    \item{prepanel}{A function that takes the same arguments as the \dQuote{panel}}
    \item{grid}{Default manner of drawing grid lines - for custom grids, use type = c('p','g') and set the xat, yat, xgrid.at, ygrid.at parameters}
    \item{groups}{The grouping variable in the data-frame}
    \item{main}{The main plot title}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis}
    \item{main.cex}{Size of the overall plot title, defaults to 3}
    \item{xlab.cex}{Size of x-axis labels, defaults to 2.5}
    \item{ylab.cex}{Size of y-axis labels, defaults to 2.5}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xlimits}{Two-element vector giving the x-axis limits, defaults to automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, defaults to automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, defaults to automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, defaults to automatic}
    \item{xgrid.at}{Vector listing where the x-axis grid lines should be drawn, defaults to xat}
    \item{ygrid.at}{Vector listing where the y-axis grid lines should be drawn, defaults to yat}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.cex}{Size of x-axis scales, defaults to 1.5}
    \item{yaxis.cex}{Size of y-axis scales, defaults to 1.5}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.log}{Logical indicating whether x-variable should be in logarithmic scale (and what base if numeric)}
    \item{yaxis.log}{Logical indicating whether y-variable should be in logarithmic scale (and what base if numeric)}
    \item{xaxis.rot}{Counterclockwise rotation of text in x-axis scales in degrees, defaults to 0}
    \item{yaxis.rot}{Counterclockwise rotation of text in y-axis scales in degrees, defaults to 0}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{type}{Plot type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point/line colour}
    \item{lwd}{Specifies line width, defaults to 1}
    \item{lty}{Specifies line style, defaults to 1 (solid)}
    \item{axis.lwd}{Thickness of width of axis lines}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
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
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\author{Ying Wu}
\seealso{\code{\link[lattice]{qq}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{

set.seed(12345);

create.qqplot.comparison(
    filename = 'QQcomparison_Simple.tiff',
    x = list(rnorm(100), rnorm(100))
    );

# Minimal Input
create.qqplot.comparison(
    filename = 'QQcomparison_Minimal_Input.tiff',
    x = list(microarray[1:500,2], microarray[1:500,2]),
    main = 'Minimal input',
    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

# Axes & Labels
create.qqplot.comparison(
    filename = 'QQcomparison_Axes_Labels.tiff',
    x = list(microarray[1:500,2], microarray[1:500,2]),
    main = 'Axes & labels',
    # adding axes and labels
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # adding grid for good measure
    grid = TRUE,
    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

# Formula input

# 'Formula' format of data
chr.locations <- microarray$Chr[1:500];
chr.locations <- replace(chr.locations, which(chr.locations == 1), 'Chromosome 1');
chr.locations <- replace(chr.locations, which(chr.locations == 2), 'Chromosome 2');

qqplot.data <- data.frame(
    sample = c(rep("Sample 1", 500), rep("Sample 2", 500)),
    value = c(microarray[1:500,1], microarray[1:500,2]),
    chr = chr.locations
    );


create.qqplot.comparison(
    filename = 'QQcomparison_Formula.tiff',
    # Using a different input method
    x = sample ~ value,
    data = qqplot.data,
    main = 'Formula input',
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xaxis.lab = seq(0, 15, 5),
    yaxis.lab = seq(0, 15, 5),
    xlimits = c(0, 17),
    ylimits = c(0, 17),
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    grid = TRUE,
    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

# Groups & Legend
create.qqplot.comparison(
 filename = 'QQcomparison_Groups_Legend.tiff',
    x = sample ~ value,
    data = qqplot.data,
    # Using fake grouping for the sake of illustration
    groups = qqplot.data$chr,
    # Set colours to differente the gruops
    col = default.colours(3),
    # Setting different plotting characters
    pch = c(15, 16),
    main = 'Groups & legend',
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    grid = TRUE,
    # Adding legend to explain groups
    key = list(
        text = list(
            lab = c('1','2'),
            cex = 1.5,
            col = 'black'
            ),
        points = list(
            pch = c(15, 16),
            col = default.colours(2),
            cex = 1
            ),
        x = 0.04,
        y = 0.95,
        padding.text = 2
        ),
    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

# Multiple qq plots
create.qqplot.comparison(
    filename = 'QQcomparison_Multiple.tiff',
    x = sample ~ value | chr,
    data = qqplot.data,
    main = 'Multiple plots',
    xlab.label = "Sample 1",
    ylab.label = "Sample 2",
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    grid = TRUE,
    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

# Nature style
create.qqplot.comparison(
    filename = 'QQcomparison_Nature_style.tiff',
    x = sample ~ value,
    data = qqplot.data,
    main = 'Nature style',
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    grid = TRUE,

   # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.label = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.label = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3)),

    description = 'QQplot comparison created by BoutrosLab.plotting.general'
    );

}
\keyword{hplot}
