\name{legend.grob}
\alias{legend.grob}
\title{Generate a legend grob}
\description{Takes a list and generates a grob representing one or more legends}
\usage{
legend.grob(
	legends,
	label.cex = 1,
	title.cex = 1,
	title.just = 'centre',
	title.fontface = 'bold',
	font.family = NULL,
	size = 3,
	border = NULL,
	border.padding = 1,
	layout = c(1, length(legends)),
	between.col = 1,
	between.row = 1,
	use.legacy.settings = FALSE,
        x = 0.5,
        y = 0.5,
	background.col = "white",
	background.alpha = 0
	);
}
\arguments{
    \item{legends}{A list defining one or more legends. Each must be a separate component called 'legend'. Each component is a list with components 'colours', 'labels', 'border' (optional), 'title' (optional), and 'size' (optional).\cr
The 'colours' component is a vector of fill colours to be used for the rectangles, the 'labels' component is a vector of text labels corresponding to the colours, the 'border' component specifies the colours of the rectangle borders (defaults to black), and the 'title' component is a character string representing a title for the legend.}
    \item{label.cex}{Size of text labels in the legends, defaults to 1.}
    \item{title.cex}{Size of titles in the legends, defaults to 1.}
    \item{title.just}{Justification of titles in the legends. Defaults to 'centre'.}
    \item{title.fontface}{Font face of titles in the legends ('plain', 'bold', 'italic', etc.)}
    \item{font.family}{Font to be used for legend text. If NULL, the default font is used.}
    \item{size}{Width of the legend boxes in 'character' units. If a 'size' argument is specified for a legend component, it will override this value.}
    \item{border}{A list of parameters (passed to \code{gpar}) specifying line options for the legend border. If NULL, no border is drawn.}
    \item{border.padding}{The amount of empty space (split equally on both sides) to add between the legend and its border, in 'lines' units. Defaults to 1.}
    \item{layout}{Numeric vector of length 2 specifying the number of columns and rows for the legend layout. Defaults to a 1-column layout. Note that legends are added to the layout in a row-wise order.}
    \item{between.col}{Amount of space to add between columns in the layout, in 'lines' units. Defaults to 0.5.}
    \item{between.row}{Amount of space to add between rows in the layout, in 'lines' units. Defaults to 0.5.}
    \item{use.legacy.settings}{boolean to set wheter or not to use legacy mode settings (font)}
    \item{x}{x coordinate in npc coordinate system}
    \item{y}{y coordinate in npc coordinate system}
    \item{background.col}{colour for the background of the legend grob}
    \item{background.alpha}{alpha for the background of the legend grob}
}
\value{Returns an grob representing the legend(s)}
\author{Lauren Chong}
\seealso{\code{\link{create.heatmap}}, \code{\link{draw.key}}, \code{\link{gpar}}}
\section{Implementation}{
This function was initially created to be called from \code{create.heatmap} to draw a covariate legend.\cr
The decision to use a grob (grid graphical object) to represent the legend was made based on the format of the \code{levelplot} function in the \code{lattice} package.  Since the \code{legend} argument of the function requires grobs, it was easiest to create a grob to represent the legend and then, if necessary, add this to any existing grobs (dendrograms, etc.) in the \code{create.heatmap} function using a grid layout.\cr
An alternative method of creating the legend using the \code{barchart} function was tested, but it was unclear how to merge this barchart with the heatmap since the \code{c.trellis} function attempts to unify the format of the two images, and the use of viewports required that the plots be drawn, eliminating the possibility of suppressing output and saving the final graph as a trellis object.
}
\examples{
# The 'cairo' graphics is preferred but on M1 Macs this is not available
bitmap.type = getOption('bitmapType')
if (capabilities('cairo')) {
	bitmap.type <- 'cairo';
	}

# create list representing two legends
legends1 <- list(
    legend = list(
        colours = c('orange', 'chartreuse4', 'darkorchid4'),
        labels = c('Group 1', 'Group 2', 'Group 3'),
        border = c('orange', 'chartreuse4', 'darkorchid4'),
        title = 'Legend #1'
        ),
    legend = list(
        colours = c('firebrick3', 'lightgrey'),
        labels = c('Case', 'Control')
        )
    );

# create a legend grob using defaults
legend.grob1 <- legend.grob(
    legends = legends1
    );
tiff(
    filename = tempfile(pattern = 'legend_grob1', fileext = '.tiff'),
    type = bitmap.type,
    width = 5,
    height = 5,
    units = 'in',
    res = 800,
    compression = 'lzw'
    );
grid.draw(legend.grob1);
dev.off();

# create the same legend with some customizations
legend.grob2 <- legend.grob(
    legends = legends1,
    label.cex = 1.25,
    title.cex = 1.25,
    title.just = 'left',
    title.fontface = 'bold.italic',
    size = 4,
    border = list(),
    layout = c(2,1)
    );
tiff(
    filename = tempfile(pattern = 'legend_grob2', fileext = '.tiff'),
    type = bitmap.type,
    width = 5,
    height = 5,
    units = 'in',
    res = 800,
    compression = 'lzw'
    );
grid.draw(legend.grob2);
dev.off();

# create a legend where the title is underlined (see ?plotmath), add space between rows
legends2 <- list(
    legend = list(
        colours = c('orange', 'chartreuse4', 'darkorchid4'),
        labels = c('Group 1', 'Group 2', 'Group 3'),
        title = expression(underline('Legend #1'))
        ),
    # Use dots instead of rectangles
    point = list(
        colours = c('firebrick3', 'lightgrey'),
        labels = c('A label', 'A longer label'),
        # Set dot size
        cex = 1.5
        )
    );

# create the new legend and use more complex border
legend.grob3 <- legend.grob(
    legends = legends2,
    border = list(col = 'blue', lwd = 2, lty = 3),
    border.padding = 1.5,
    between.row = 3
    );
tiff(
    filename = tempfile(pattern = 'legend_grob3', fileext = '.tiff'),
    type = bitmap.type,
    width = 5,
    height = 5,
    units = 'in',
    res = 800,
    compression = 'lzw'
    );
grid.draw(legend.grob3);
dev.off();

# Make a legend where the size of boxes is customized
legends3 <- list(
    legend = list(
        colours = c('orange', 'chartreuse4', 'darkorchid4'),
        labels = c('Group 1', 'Group 2', 'Group 3'),
        title = 'Legend #1',
        size = c(3,2,1)
        ),
    legend = list(
        colours = NULL,
        labels = c('+', '-'),
        border = 'transparent',
        title = 'Disease status',
        size = 0.5
        )
    );
legend.grob4 <- legend.grob(
    legends = legends3
    );
tiff(
    filename = tempfile(pattern = 'legend_grob4', fileext = '.tiff'),
    type = bitmap.type,
    width = 5,
    height = 5,
    units = 'in',
    res = 800,
    compression = 'lzw'
    );
grid.draw(legend.grob4);
dev.off();
}
