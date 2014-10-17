\name{create.dotmap}
\alias{create.dotmap}
\title{Make a dotmap with coloured background}
\description{Takes two data.frames and creates a dotmap with a coloured background. A dotmap is an ordered array of evenly-spaced dots whose size and colour can be user-specified to represent characteristics. For example, size gives the absolute magnitude of the correlation and colour gives the sign of the correlation. The coloured background may indicate p-values.}
\usage{
create.dotmap(x, bg.data = NULL, filename = NULL, main = NULL, pch = 19, 
pch.border.col = 'black', add.grid = TRUE, xaxis.lab = colnames(x), 
yaxis.lab = rownames(x), xaxis.rot = 0, yaxis.rot = 0, main.cex = 2, 
xlab.cex = 2, ylab.cex = 2, xlab.label = NULL, ylab.label = NULL, 
xlab.col = 'black', ylab.col = 'black', xaxis.cex = 2.5, yaxis.cex = 2.5, 
xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1, 
axis.top = 1, axis.bottom = 1, axis.left = 1, axis.right = 1, 
top.padding = 0.1, bottom.padding = 0.7, right.padding = 0.1, 
left.padding = 0.5, key = list(text = list(lab = c(''))), legend = NULL, 
grid.col.lwd = 1.5, grid.row.lwd = 1.5, spot.size.function = 'default', 
spot.colour.function = 'default', NA.spot.size = 7, NA.pch = 4, grid.colour = NULL, 
colour.scheme = 'white', total.colours = 99, at = NULL, colour.centering.value = 0, 
colourkey = FALSE, colourkey.labels.at = NULL, colourkey.labels = NULL, colourkey.cex = 1,
colour.alpha = 1, bg.alpha = 0.5, fill.colour = 'white', key.top = 0.1, 
height = 6, width = 6, size.units = 'in', resolution = 1600, 
enable.warnings = FALSE, col.colour = 'black', row.colour = 'black', 
description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, 
ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, 
col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', 
yaxis.fontface = 'bold', dot.colour.scheme = NULL, style = 'BoutrosLab');
}
\arguments{
    \item{x}{An unstacked data.frame to plot the dotmap}
    \item{bg.data}{An unstacked data.frame to plot the background, of the same size as \dQuote{x}. Column names specified here may be arbitrary: they are not used in the plot.}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{pch}{Plotting character}
    \item{pch.border.col}{Colour of the dot border if using pch = 21:25}
    \item{add.grid}{Should a grid of black-lines separating each column/row be added?}
    \item{main}{The main plot title (space is reclaimed if NULL)}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis}
    \item{xlab.col}{Colour of the x-axis label, defaults to \dQuote{black}}
    \item{ylab.col}{Colour of the y-axis label, defaults to \dQuote{black}}
    \item{main.cex}{Character expansion for the main title, defaults to 2}
    \item{xlab.cex}{Size of x-axis label, defaults to 2}
    \item{ylab.cex}{Size of y-axis label, defaults to 2}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to colnames(x)}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to rownames(x)}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 1.2}
    \item{yaxis.cex}{Size of y-axis tick labels, defaults to 1.5}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to \dQuote{black}}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to \dQuote{black}}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{axis.top}{Specifies the padding on the top of the plot}
    \item{axis.bottom}{Specifies the padding on the bottom of the plot}
    \item{axis.left}{Specifies the padding on the left of the plot}
    \item{axis.right}{Specifies the padding on the right of the plot}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.1}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 0.7}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 0.1}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 0.5}
    \item{key}{A list giving the key (legend). The default suppresses drawing. If the key has a \dQuote{space} component then extra space will be cleared on that side of the plot for the key}
    \item{legend}{Add a legend to the plot. Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{grid.col.lwd}{Thickness of column grid lines}
    \item{grid.row.lwd}{Thickness of row grid lines}
    \item{spot.size.function}{The function that translates values in x into dotmap spot-size. The default is 0.1 + (2 * abs(x))}
    \item{spot.colour.function}{The function that translates values in x into dotmap spot-colour. The default gives negative values blue, positive values red, and zero white}
    \item{NA.spot.size}{The size for plotting character for NA cells. Defaults to 7.}
    \item{NA.pch}{The type of plotting character to represent NA cells. Defaults to 4 ('X').}
    \item{grid.colour}{The colour for the grid lines. DEPRECATED}
    \item{colour.scheme}{Background colouring. Accepts a vector of colours. Vectors of two or three colours are gradiated to create the final palette. Defaults to \dQuote{white}.}
    \item{total.colours}{Total number of colours to plot for the Background colours}
    \item{at}{A vector specifying the breakpoints along the range of bg; each interval specified by these breakpoints are assigned to a colour from the palette. Defaults to NULL, which corresponds to the range of bg being divided into total.colours equally spaced intervals. If bg has values outside of the range specified by \dQuote{at}, those values are shown with colours corresponding to the extreme ends of the colour spectrum and a warning is given.}
    \item{colour.centering.value}{What should be the center of the background key}
    \item{colourkey}{Determines if the colour key should be added or not and sets up its formatting. Defaults to FALSE.}
    \item{colourkey.labels.at}{A vector specifying the tick-positions on the background colourkey}
    \item{colourkey.labels}{A vector specifying tick-labels of the background colourkey}
    \item{colourkey.cex}{Size of the background colourkey label text}
    \item{colour.alpha}{Bias to be added to background colour selection (uses x^colour.alpha in mapping)}
    \item{bg.alpha}{The alpha value of the background colours, defaults to 0.5 so that the background does not compete with the dot colours for attention.}
    \item{fill.colour}{The background fill colour (only exposed where missing values are present). Defaults to white. NOTE: If you change this colour, you may want to set bg.alpha to 1 to avoid the fill colour showing through}
    \item{key.top}{A number specifying the distance at top of key, defaults to 0.1}
    \item{height}{Figure height in size.units}
    \item{width}{Figure width in size.units}
    \item{size.units}{Units of size for the figure}
    \item{resolution}{Figure resolution in dpi}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{col.colour}{The colour for the column grid lines, defaults to \dQuote{black}. Can be a vector.}
    \item{row.colour}{The colour for the row grid lines, defaults to \dQuote{black}. Can be a vector.}
    \item{description}{Description of image/plot; default NULL.}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{dot.colour.scheme}{Colour Scheme for the dots}
    \item{style}{defaults to \dQuote{BoutrosLab}, also accepts \dQuote{Nature}, which changes parameters according to Nature formatting requirements}
    }
\details{It would be nice to have a library of suitable spot.size and spot.colour functions. 
    \preformatted{
    Earlier ideas included: 
    (1) Changing the dot shape to triangles, so that upward or downward-pointing dots indicated direction of change. This would allow dot colour to be used to encode something else. This idea was not used because in the case of very small dots, the direction of the triangle might not be visible. 
    (2) Adding arrows above or below dots to indicate direction of change. This idea was not used because there may not always be enough space present to add such arrows. 
    (3) Adding line(s) in the background set at different angles to show data. This was found to be not intuitive to read.

A future addition may be to add the option of outlining boxes instead of adding a background. This would be applicable in cases where there is very little background space, and consequently, the background colour would not be very visible.
    }
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
\seealso{\code{\link[lattice]{xyplot}}, \code{\link[lattice]{levelplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
simple.data <- data.frame(
    'A' = runif(n = 15, min = -1, max = 1),
    'B' = runif(n = 15, min = -1, max = 1),
    'C' = runif(n = 15, min = -1, max = 1),
    'D' = runif(n = 15, min = -1, max = 1),
    'E' = runif(n = 15, min = -1, max = 1)
    );

create.dotmap(
    filename = 'Dotmap_Simple.tiff',
    x = simple.data,
    main = 'Simple',
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# create a function to determine the spot sizes (default function works best with values < 1)
spot.size.med <- function(x) {abs(x)/3;}

# Minimal Input
create.dotmap(
    filename = 'Dotmap_Minimal_Input.tiff',
    x = microarray[1:5,1:5],
    main = 'Minimal input',
    spot.size.function = spot.size.med,
    xaxis.rot = 90,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Axes & Labels
spot.size.small <- function(x) {abs(x)/5;}

create.dotmap(
    filename = 'Dotmap_Axes_Labels.tiff',
    x = microarray[1:15,1:15],
    main = 'Axes & labels',
    spot.size.function = spot.size.small,
    # Adjusting the font sizes and labels
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Legend
key.sizes <- seq(2,12,2);

create.dotmap(
    filename = 'Dotmap_Legend.tiff',
    x = microarray[1:15,1:15],
    main = 'Legend',
    spot.size.function = spot.size.small,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    # Legend for dots
    key = list(
        space = 'right',
        points = list(
            cex = spot.size.small(key.sizes),
            col = default.colours(2, palette.type = 'dotmap')[2],
            pch = 19
            ),
        text = list(
            lab = as.character(key.sizes),
            cex = 1,
            adj = 1
            ),
        padding.text = 3,
        background = 'white'
        ),
    key.top = 1,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Cluster by dots and add dendrogram
plot.data <- microarray[1:15,1:15];

# cluster data
clustered.data <- diana(plot.data);

# order data by cluster
plot.data <- plot.data[clustered.data$order,];

# create dendogram
dendrogram.data <- create.dendrogram(x = plot.data, clustering.method = 'diana',
	cluster.dimension = 'row');

dendrogram.grob <- latticeExtra::dendrogramGrob(
        x = dendrogram.data,
        side = 'right',
        type = 'rectangle'
        );

# create dotmap
create.dotmap(
    x = plot.data,
    filename = 'Dotmap_clustered_dendrogram.tiff',
    main = 'Clustered & dendrogram',
    spot.size.function = spot.size.small,
    # Adjusting the font sizes and labels
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    legend = list(
        right = list(fun = dendrogram.grob)
        ),
    right.padding = 4,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Add background data
key.sizes <- c(-1, 1);

CNA.colour.function <- function(x){
    colours <- rep('white', length(x));
    colours[sign(x) == 1] <- 'Red';
    colours[sign(x) == -1] <- 'Blue';
    colours[x == 0] <- 'transparent';
    return(colours);
    }

create.dotmap(
    filename = 'Dotmap_with_Background.tiff',
    # added new data for the dots
    x = CNA[1:15,1:15],
    # Moving the dot-data to be background data
    bg.data = microarray[1:15,1:15],
    colour.scheme = c('white','black'),
    main = 'Background',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    key = list(
        space = 'right',
        points = list(
            cex = 1,
            col = CNA.colour.function(key.sizes),
            pch = 19
            ),
        text = list(
            lab = c('Gain', 'Loss'),
            cex = 1,
            adj = 1
            ),
        title = 'CNA',
        padding.text = 2,
        background = 'white'
        ),
    # Adding colourkey for background data
    colourkey = TRUE,
    key.top = 1,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Discrete background colours
create.dotmap(
    filename = 'Dotmap_Discrete_Background.tiff',
    x = CNA[1:15,1:15],
    bg.data = microarray[1:15,1:15],
    main = 'Discrete background',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    key = list(
        space = 'right',
        points = list(
            cex = 1,
            col = CNA.colour.function(key.sizes),
            pch = 19
            ),
        text = list(
            lab = c('Gain', 'Loss'),
            cex = 1,
            adj = 1
            ),
        title = 'CNA',
        padding.text = 2,
        background = 'white'
        ),
    colourkey = TRUE,
    key.top = 1,
    # Changing background colour scheme
    colour.scheme = c('lightyellow','gold','darkorange', 'darkorange3'),
    at = seq(0,12,3),
    colourkey.labels = seq(0,12,3),
    colourkey.labels.at = seq(0,12,3),
    bg.alpha = 0.65,
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Dot outlines
border.colours <- function(x){
    colours <- rep('transparent', length(x));
    colours[x > 0] <- 'black';
    colours[x == 0] <- 'transparent';
    return(colours);
    }

create.dotmap(
    filename = 'Dotmap_Outlined_Dots.tiff',
    x = CNA[1:15,1:15],
    bg.data = microarray[1:15,1:15],
    main = 'Dot outlines',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    key = list(
        space = 'right',
        points = list(
            cex = 1,
            col = 'black',
            # Remember to also change the pch in the legend
            pch = 21,
            fill = CNA.colour.function(key.sizes)
            ),
        text = list(
            lab = c('Gain', 'Loss'),
            cex = 1,
            adj = 1
            ),
        title = 'CNA',
        padding.text = 2,
        background = 'white'
        ),
    colourkey = TRUE,
    key.top = 1,
    colour.scheme = c('lightyellow','gold','darkorange', 'darkorange3'),
    at = seq(0,12,3),
    colourkey.labels = seq(0,12,3),
    colourkey.labels.at = seq(0,12,3),
    bg.alpha = 0.65,
    # Change the plotting character to one which has an outline
    pch = 21,
    pch.border.col = border.colours(CNA[1:15,1:15]),
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Covariates & Legend
sex.colours <- patient$sex[1:15];
sex.colours[sex.colours == 'male'] <- 'dodgerblue';
sex.colours[sex.colours == 'female'] <- 'pink';

sample.covariate <- list(
    rect = list(
        col = 'black',
        fill = sex.colours,
        lwd = 1.5
        )
    );

cov.grob <- covariates.grob(
    covariates = sample.covariate,
    ord = c(1:15),
    side = 'top'
    );

sample.cov.legend <- list(
    legend = list(
        colours = c('dodgerblue', 'pink'),
        labels = c('male','female'),
        title = 'Sex'
        )
    );

cov.legend <- legend.grob(
    legends = sample.cov.legend
    );

create.dotmap(
    filename = 'Dotmap_Covariates.tiff',
    x = CNA[1:15,1:15],
    bg.data = microarray[1:15,1:15],
    main = 'Covariates',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    key = list(
        space = 'right',
        points = list(
            cex = 1,
            col = 'black',
            pch = 21,
            fill = CNA.colour.function(key.sizes)
            ),
        text = list(
            lab = c('Gain', 'Loss'),
            cex = 1,
            adj = 1
            ),
        title = 'CNA',
        padding.text = 2,
        background = 'white'
        ),
    colourkey = TRUE,
    key.top = 1,
    colour.scheme = c('lightyellow','gold','darkorange', 'darkorange3'),
    at = seq(0,12,3),
    colourkey.labels = seq(0,12,3),
    colourkey.labels.at = seq(0,12,3),
    bg.alpha = 0.65,
    pch = 21,
    pch.border.col = border.colours(CNA[1:15,1:15]),
    # Insert covariates & legend
    legend = list(
        top = list(
            fun = cov.grob
            ),
        left = list( 
            fun = cov.legend
            )
        ),
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# Side covariates with label
chr.cov.colours <- microarray$Chr;
chr.cov.colours[microarray$Chr == 1] <- default.colours(3, palette.type = 'chromosomes')[1];
chr.cov.colours[microarray$Chr == 2] <- default.colours(3, palette.type = 'chromosomes')[2];
chr.cov.colours[microarray$Chr == 3] <- default.colours(3, palette.type = 'chromosomes')[3];

chr.covariate <- list(
    rect = list(
        col = 'white',
        fill = chr.cov.colours,
        lwd = 1.5
        )
    );

chr.cov.grob <- covariates.grob(
    covariates = chr.covariate,
    ord = c(1:15),
    side = 'right'
    );

# create dot legend
dot.grob <- draw.key(
    list(
        space = 'right',
         points = list(
                cex = 1,
                col = 'black',
                pch = 21,
                fill = CNA.colour.function(key.sizes)
                ),
            text = list(
                lab = c('Gain', 'Loss'),
                cex = 1,
                adj = 1
                ),
            title = 'CNA',
            padding.text = 2,
            background = 'white'
        )
    );

# Setting up the layout for the joint legends
right.layout <- grid.layout(
    nrow = 1,
    ncol = 2,
    width = unit(
        x = c(0,1),
        units = rep('lines',2)
        ),
    heights = unit(
        x = c(1,1),
        units = rep('npc', 1)
        )
    );

right.grob <- frameGrob(layout = right.layout);

right.grob <- packGrob(
    frame = right.grob,
    grob = chr.cov.grob,
    row = 1,
    col = 1
    );

right.grob <- packGrob(
    frame = right.grob,
    grob = dot.grob,
    row = 1,
    col = 2
    );

temp <- create.dotmap(
    filename = 'Dotmap_Covariates_Side.tiff',
    x = CNA[1:15,1:15],
    bg.data = microarray[1:15,1:15],
    main = 'Both covariates',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.label = 'Sample',
    ylab.label = 'Gene',
    xlab.cex = 1,
    ylab.cex = 1,
    colourkey = TRUE,
    key.top = 1,
    colour.scheme = c('lightyellow','gold','darkorange', 'darkorange3'),
    at = seq(0,12,3),
    colourkey.labels = seq(0,12,3),
    colourkey.labels.at = seq(0,12,3),
    bg.alpha = 0.65,
    pch = 21,
    pch.border.col = border.colours(CNA[1:15,1:15]),
    # insert covariates & legend
    legend = list(
        right = list(
            fun = right.grob
            )
        ),
    description = 'Dotmap created by BoutrosLab.plotting.general'
    );

# add side label to covariate
print(temp, position = c(0,0,1,1), more = TRUE);

draw.key(
    key = list(
        text = list(
            lab = 'Covariate Label',
            cex = 1,
            adj = 1
            )
        ),
    # position label on the plot
    vp = viewport(x = 0.86, y = 0.155, height = 1, width = 0.5, angle = 90),
    draw = TRUE
    )

# Nature style
create.dotmap(
    filename = 'Dotmap_Nature_style.tiff',
    x = CNA[1:15,1:15],
    bg.data = microarray[1:15,1:15],
    main = 'Nature style',
    spot.size.function = 1,
    spot.colour.function = CNA.colour.function,
    xaxis.cex = 0.8,
    yaxis.cex = 0.8,
    xaxis.lab = 1:15,
    xlab.cex = 1,
    ylab.cex = 1,
    key = list(
        space = 'right',
        points = list(
            cex = 1,
            col = 'black',
            # Remember to also change the pch in the legend
            pch = 21,
            fill = CNA.colour.function(key.sizes)
            ),
        text = list(
            lab = c('Gain', 'Loss'),
            cex = 1,
            adj = 1
            ),
        title = 'CNA',
        padding.text = 2,
        background = 'white'
        ),
    colourkey = TRUE,
    key.top = 1,
    colour.scheme = c('lightyellow','gold','darkorange', 'darkorange3'),
    at = seq(0,12,3),
    colourkey.labels = seq(0,12,3),
    colourkey.labels.at = seq(0,12,3),
    bg.alpha = 0.65,
    # Change the plotting character to one which has an outline
    pch = 21,
    pch.border.col = border.colours(CNA[1:15,1:15]),

    # set style to Nature 
    style = 'Nature',
    
    # demonstrating how to italicize character variables
    ylab.lab = expression(paste('italicized ', italic('a'))),
  
    # demonstrating how to create en-dashes
    xlab.lab = expression(paste('en dashs: 1','\u2013', '10'^'\u2013', ''^3))
    );

}
\keyword{hplot}
