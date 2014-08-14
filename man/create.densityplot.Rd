\name{create.densityplot}
\alias{create.densityplot}
\title{Make a density plot}
\description{Takes a list of vectors and creates a density-plot with each vector as a separate curve}
\usage{
create.densityplot(x, filename = NULL, xlab.label = NULL, main = NULL, type = "l", 
lty = 'solid', cex = 0.75, pch = 19, col = 'black', lwd = 2, xlimits = NULL, 
ylimits = NULL, xat = TRUE, yat = TRUE, xgrid.at = xat, ygrid.at = yat, 
xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 1, yaxis.cex = 1, xaxis.rot = 0, 
yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, 
yaxis.tck = 1, main.cex = 2, xlab.cex = 2, ylab.cex = 2, ylab.label = 'Density', 
xlab.col = 'black', ylab.col = 'black', key = list(text = list(lab = c(''))), 
legend = NULL, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5, 
right.padding = 0.1, add.axes = FALSE, abline = NULL, height = 6, 
width = 6, size.units = 'in', resolution = 1500, enable.warnings = FALSE, 
description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, 
ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, 
col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', 
yaxis.fontface = 'bold');
}
\arguments{
    \item{x}{A list of vectors, each of which will be plotted as a separate curve in the final plot}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis, defaults to \dQuote{Density}}
    \item{main}{The main plot title (space is reclaimed if NULL)}
    \item{type}{Plot type}
    \item{lty}{Line type}
    \item{cex}{Character expansion for plotting symbol}
    \item{pch}{Plotting character}
    \item{col}{Point/line colour}
    \item{lwd}{Thickness of width of any best-fit lines}
    \item{xlimits}{Two-element vector giving the x-axis limits, defaults to automatic}
    \item{ylimits}{Two-element vector giving the y-axis limits, defaults to automatic}
    \item{xat}{Vector listing where the x-axis labels should be drawn, defaults to automatic}
    \item{yat}{Vector listing where the y-axis labels should be drawn, defaults to automatic}
    \item{xgrid.at}{Vector listing where the x-axis grid lines should be drawn, defaults to xat}
    \item{ygrid.at}{Vector listing where the y-axis grid lines should be drawn, defaults to yat}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 1}
    \item{yaxis.cex}{Size of x-axis tick labels, defaults to 1}
    \item{main.cex}{Size of text for main plot title, defaults to 2}
    \item{xlab.cex}{Size of x-axis label, defaults to 2}
    \item{ylab.cex}{Size of y-axis label, defaults to 2}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{top.padding}{A number giving the top padding in multiples of the lattice default}
    \item{bottom.padding}{A number giving the bottom padding in multiples of the lattice default}
    \item{left.padding}{A number giving the left padding in multiples of the lattice default}
    \item{right.padding}{A number giving the right padding in multiples of the lattice default}
    \item{add.axes}{Allow axis lines to be turned on or off}
    \item{abline}{A list to be passed through \code{\link{panel.abline}} adding lines to the plot}
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
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
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
    filename = 'Densityplot_Simple.tiff',
    x = simple.data,
    main = 'Simple',
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# format data
format.data <- microarray[1:3,1:58];
format.data <- as.data.frame(t(format.data));

# Minimal Input
create.densityplot(
    filename = 'Densityplot_Minimal_Input.tiff',
    x = format.data,
    main = 'Minimal input',
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Line type
create.densityplot(
    filename = 'Densityplot_Line_Type.tiff',
    x = format.data,
    main = 'Line type',
    # Line type
    lty = c('solid','dashed','dotted'),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Axes & Labels
create.densityplot(
    filename = 'Densityplot_Axes_Labels.tiff',
    x = format.data,
    main = 'Axes & labels',
    lty = c('solid','dashed','dotted'),
    # Axes & Labels
    ylimits = c(-0.1, 2.5),
    ylab.cex = 1.5,
    xat = seq(0, 13, 1),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Colour change & Legend
create.densityplot(
    filename = 'Densityplot_Colour_Legend.tiff',
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
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Correlation key
create.densityplot(
    filename = 'Densityplot_Correlation_Key.tiff',
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
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Gridlines
create.densityplot(
    filename = 'Densityplot_Gridlines_1.tiff',
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
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Gridlines
create.densityplot(
    filename = 'Densityplot_Gridlines_2.tiff',
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
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

}
\keyword{hplot}
