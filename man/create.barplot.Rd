\name{create.barplot}
\alias{create.barplot}
\title{Make a barplot}
\description{Takes a data.frame and creates a barplot}
\usage{
create.barplot(formula, data, groups = NULL, stack = FALSE, 
filename = NULL, main = NULL, abline.h = NULL, abline.v = NULL, 
abline.type = NULL, abline.lwd = NULL, abline.col = 'black', draw.grid = FALSE, 
xgrid.at = xat, ygrid.at = yat, grid.lwd = 5, xlab.label = NULL, ylab.label = NULL, 
xlab.col = 'black', ylab.col = 'black', xaxis.lab = TRUE, yaxis.lab = TRUE, 
xaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', 
yaxis.col = 'black', xaxis.cex = 1.2, yaxis.cex = 1.5, xaxis.rot = 0, yaxis.rot = 0, 
xaxis.tck = 1, yaxis.tck = 1, main.cex = 2.5, xlab.cex = 2, ylab.cex = 2, 
ylimits = NULL, yat = TRUE, xlimits = NULL, xat = TRUE, x.spacing = 0, 
y.spacing = 0, top.padding = 0.5, bottom.padding = 1, right.padding = 1, 
left.padding = 1, key.bottom = 0.1, ylab.axis.padding = 0, 
axis.xlab.padding = 0.5, x.relation = 'same', y.relation = 'same', 
layout = NULL, as.table = FALSE, col = 'black', strip.col = 'white', 
strip.cex = 1, background.col = 'transparent', border.col = 'black', y.error.up = NULL, 
y.error.down = y.error.up, y.error.bar.col = 'black', 
error.whisker.width = width/(nrow(data)*4), error.bar.lwd = 1, error.whisker.angle = 90,
add.background.shading = FALSE, background.shading.xpos = NULL, 
background.shading.ypos = NULL, background.shading.colour = 'grey85', 
raster = NULL, raster.vert = TRUE, raster.just = 'center', 
raster.width.dim = unit(2/37, 'npc'), key = list(text = list(lab = c(''))),
legend = NULL, lwd = 1, plot.horizontal = FALSE, height = 6, width = 6, 
size.units = 'in', resolution = 1000, enable.warnings = FALSE, description = NULL,
add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, 
xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', 
alpha.rectangle = 1, origin = 0, reference = TRUE, sample.order = 'none', 
group.labels = FALSE, line.func = NULL, line.from = 0, line.to = 0, 
line.col = 'transparent', line.infront = TRUE,
text.above.bars = list(labels = NULL, padding = NULL, bar.locations = NULL, rotation = 0)
);
}
\arguments{
    \item{formula}{The formula used to extract the x & y components from the data-frame}
    \item{data}{The data-frame to plot}
    \item{groups}{Optional grouping variable.  Expression or variable.}
    \item{stack}{Logical, relevant when groups is non-null. If FALSE (the default), bars for different values of the grouping variable are drawn side by side, otherwise they are stacked}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself}
    \item{main}{The main plot title (space is reclaimed if NULL)}
    \item{abline.h}{Specify the superimposed horizontal line(s)}
    \item{abline.v}{Specify the superimposed vertical line(s)}
    \item{abline.type}{Specify the superimposed line type}
    \item{abline.lwd}{Specify the superimposed line width}
    \item{abline.col}{Specify the superimposed line colour (defaults to black)}
    \item{draw.grid}{Specify whether to draw grid or not (defaults to FALSE)}
    \item{xgrid.at}{Specify where to draw x-axis grid lines (defaults to xat)}
    \item{ygrid.at}{Specify where to draw y-axis grid lines (defaults to yat)}
    \item{grid.lwd}{Specify width of grid line (defaults to 5)}
    \item{xlab.label}{The label for the x-axis}
    \item{ylab.label}{The label for the y-axis}
    \item{xaxis.lab}{Vector listing x-axis tick labels, defaults to automatic}
    \item{yaxis.lab}{Vector listing y-axis tick labels, defaults to automatic}
    \item{xaxis.cex}{Size of x-axis tick labels, defaults to 1.2}
    \item{yaxis.cex}{Size of y-axis tick labels, defaults to 1.5}
    \item{xaxis.rot}{Rotation of x-axis tick labels; defaults to 0}
    \item{yaxis.rot}{Rotation of y-axis tick labels; defaults to 0}
    \item{xaxis.col}{Colour of the x-axis tick labels, defaults to black}
    \item{yaxis.col}{Colour of the y-axis tick labels, defaults to black}
    \item{main.cex}{Size of text for main plot title, defaults to 2.5}
    \item{xlab.cex}{Size of x-axis label, defaults to 2}
    \item{ylab.cex}{Size of y-axis label, defaults to 2}
    \item{xlab.col}{Colour of the x-axis label, defaults to black}
    \item{ylab.col}{Colour of the y-axis label, defaults to black}
    \item{xaxis.fontface}{Fontface for the x-axis scales}
    \item{yaxis.fontface}{Fontface for the y-axis scales}
    \item{xaxis.tck}{Specifies the length of the tick marks for x-axis, defaults to 1}
    \item{yaxis.tck}{Specifies the length of the tick marks for y-axis, defaults to 1}
    \item{ylimits}{A list of vectors listing what the y-axis limits should be for each subplot. Defaults to NULL to let R figure out the limits}  
    \item{yat}{A list of vectors listing where the y-axis labels should be drawn for each subplot. Defaults to TRUE to let R decide where to draw the labels}
    \item{xlimits}{A list of vectors listing what the x-axis limits should be for each subplot. Defaults to NULL to let R figure out the limits. Useful when plot.horizontal = TRUE}  
    \item{xat}{A list of vectors listing where the x-axis labels should be drawn for each subplot. Defaults to TRUE to let R decide where to draw the labels. Useful when plot.horizontal = TRUE}
    \item{x.spacing}{A number specifying the distance between panels along the x-axis, defaults to 0}
    \item{y.spacing}{A number specifying the distance between panels along the y-axis, defaults to 0}
    \item{top.padding}{A number specifying the distance to the top margin, defaults to 0.5}
    \item{bottom.padding}{A number specifying the distance to the bottom margin, defaults to 2}
    \item{right.padding}{A number specifying the distance to the right margin, defaults to 1}
    \item{left.padding}{A number specifying the distance to the left margin, defaults to 1}
    \item{key.bottom}{A number specifying how much space should be left for the key at the bottom, defaults to 0.1}
    \item{ylab.axis.padding}{A number specifying the distance of y-axis label to the y-axis, defaults to 0}, 
    \item{axis.xlab.padding}{A number specifying the distance of x-axis label to the x-axis, defaults to 0.5. Named differently than ylab.axis.padding because these are lattice's internal names for these values}
    \item{x.relation}{Allows x-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{y.relation}{Allows y-axis scales to vary if set to \dQuote{free}, defaults to \dQuote{same}}
    \item{layout}{A vector specifying the number of columns, rows (e.g., c(2,1). Default is NULL; see lattice::xyplot for more details}.  
    \item{as.table}{Specifies panel drawing order, default is FALSE which draws panels from bottom left corner, moving right then up. Set to TRUE to draw from top left corner, moving right then down}
    \item{col}{Filling colour of bars, defaults to black, does a grey-scale spectrum if !is.null(groups)}
    \item{strip.col}{Strip background colour, defaults to white}
    \item{strip.cex}{Strip title character expansion}
    \item{background.col}{Plot background colour, defaults to transparent}
    \item{border.col}{Specify border colour (defaults to black)}
    \item{y.error.up}{A vector specifying the length of the error bar going up from each point. If set to NULL (the default), error bars will not be drawn}
    \item{y.error.down}{A vector specifying the length of the error bar going down from each point. By default, it is set to y.error.up}
    \item{y.error.bar.col}{A string or vector of strings specifying the colour of the error bars. Defaults to black}
    \item{error.whisker.width}{A number specifying the width of the error bars. Defaults to a rough approximation based on the size of the data}
    \item{error.bar.lwd}{The line width of the error bars. Defaults to 1}
    \item{error.whisker.angle}{The angle of the error bar whiskers, defaults to 90. Can be changed to produce arrow-like bars}
    \item{add.background.shading}{Logical indicating whether to alternate background shade for every other breakpoint}
    \item{background.shading.xpos}{Positions on xaxis where shading should change}
    \item{background.shading.ypos}{Positions on yaxis where shading should change}
    \item{background.shading.colour}{colour of the background shading; defaults to grey85}
    \item{raster}{The image to raster over each bar - see Raster Images in R Graphics by Paul Murrell for full details}
    \item{raster.vert}{A logical indicating whether the raster is applied vertically or horizontally}
    \item{raster.just}{A word giving the justification of the raster, can be set to \dQuote{left}, \dQuote{right}, \dQuote{centre}, \dQuote{center}, \dQuote{bottom}, or \dQuote{top}}
    \item{raster.width.dim}{A unit object giving the width of the raster bar}
    \item{key}{A list giving the key (legend). The default suppresses drawing}
    \item{legend}{Add a legend to the plot.  Helpful for adding multiple keys and adding keys to the margins of the plot. See xyplot.}
    \item{lwd}{Line width, defaults to 1}
    \item{plot.horizontal}{Plot the bars horizontally}
    \item{height}{Figure height, defaults to 6 in}
    \item{width}{Figure width, defaults to 6 in}
    \item{size.units}{Figure units, defaults to inches}
    \item{resolution}{Figure resolution, defaults to 1000}
    \item{enable.warnings}{Print warnings if set to TRUE, defaults to FALSE}
    \item{description}{Description of image/plot; default NULL}
    \item{add.rectangle}{Allow a rectangle to be drawn, default is FALSE}
    \item{xleft.rectangle}{Specifies the left coordinate of the rectangle}
    \item{ybottom.rectangle}{Specifies the bottom coordinate of the rectangle}
    \item{xright.rectangle}{Specifies the right coordinate of the rectangle}
    \item{ytop.rectangle}{Specifies the top coordinate of the rectangle}
    \item{col.rectangle}{Specifies the colour of the rectangle}
    \item{alpha.rectangle}{Specifies the alpha of the rectangle}
    \item{origin}{The origin, generally 0}
    \item{reference}{Should the reference line be printed at the origin}
    \item{sample.order}{Should the bars be reordered, accepts values \dQuote{increasing} and \dQuote{decreasing}, labels will also be reordered}
    \item{group.labels}{Should the labels be grouped to the same amount of bars per column}
    \item{line.func}{Function for the line that should be drawn on top of plot}
    \item{line.from}{The starting point of the line on the plot}
    \item{line.to}{The ending point of the line on the plot}
    \item{line.col}{Colour of the line on the plot}
    \item{line.infront}{Should the line appear in front of the plot or not}
    \item{text.above.bars}{Should some form of text appear above the bars}
}
\value{If \code{filename} is \code{NULL} then returns the trellis object, otherwise creates a plot and returns a 0/1 success code.}
\author{Mehrdad Shamsi}
\section{Warning}{
If this function is called without capturing the return value, or specifying a filename, it may crash while trying to draw the histogram. In particular, if a script that uses such a call of create histogram is called by reading the script in from the command line, it will fail badly, with an error message about unavailable fonts:\cr
        \preformatted{
    Error in grid.Call.graphics("L_text", as.graphicsAnnot(x$label), x$x,  )
        Invalid font type
    Calls: print ... drawDetails.text -> grid.Call.graphics -> .Call.graphics
    }
}
\seealso{\code{\link[lattice]{barchart}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(12345);

simple.data <- data.frame(
    x = sample(1:15, 10),
    y = LETTERS[1:10]
    );

# Simple example
create.barplot(
    filename = 'Barplot_Simple.tiff',
    formula = x ~ y,
    data = simple.data,
    yat = seq(0,16,2)
    );

# set up the data
total.counts <- apply(SNV[1:15], 2, function(x){ mutation.count <- (30 - sum(is.na(x)))});
count.nonsyn <- function(x){
    mutation.count <- length(which(x == 1));
    }
nonsynonymous.SNV <- apply(SNV[1:15], 2, count.nonsyn);
other.mutations <- total.counts - nonsynonymous.SNV;

# subset the first fifteen samples
barplot.data <- data.frame(
    samples = rep(1:15, 2),
    mutation = c(rep('nonsynonymous', 15), rep('other',15)),
    values = c(nonsynonymous.SNV, other.mutations),
    sex = rep(patient$sex[1:15], 2),
    stage = rep(patient$stage[1:15], 2),
    msi = rep(patient$msi[1:15], 2)
    );

# Minimal input
create.barplot(
    filename = 'Barplot_Minimal_Input.tiff',
    formula = values ~ samples ,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Minimal input',
    # Editing the metadata
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Axes labels & limits
create.barplot(
    filename = 'Barplot_Custom_Axes.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Axes labels & limits',
    # Setting axes labels
    xlab.lab = 'Sample',
    ylab.lab = 'Nonsynonymous SNVs',
    # Setting y-axis limits and tick-mark locations
    ylimits = c(0,30),
    yat = seq(0,30,5),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Font size and font face
create.barplot(
    filename = 'Barplot_Font_Changes.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Font changes',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    # Changing font sizes
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    # Changing font type
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Sorting data
create.barplot(
    filename = 'Barplot_Sorted.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Sorted bars',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Order bars either by 'increasing' or 'decreasing'
    sample.order = 'decreasing',
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Colour changes
sex.colours <- replace(as.vector(barplot.data$sex), which(barplot.data$sex == 'male'),'dodgerblue');
sex.colours <- replace(sex.colours, which(barplot.data$sex == 'female'), 'pink');

create.barplot(
    filename = 'Barplot_Colour_Changes.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Colour changes',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Colour bars based on sex
    col = sex.colours,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Legend
create.barplot(
    filename = 'Barplot_Legend.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Legend',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    col = sex.colours,
    # Adding legend to explain bar colour-coding
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );


# Grouped barplot
create.barplot(
    filename = 'Barplot_Grouped.tiff',
    formula = values ~ samples,
    data = barplot.data,
    main = 'Grouped bar chart',
    xlab.lab = 'Samples',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Setting groups
    groups = mutation,
    col = default.colours(12, is.greyscale = FALSE)[11:12],
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 2,
                        fill = default.colours(12, is.greyscale = FALSE)[11:12]
                        ),  
                    text = list(
                        lab = c('Nonsynonymous SNV','Other SNV')
                        ),
                    padding.text = 3,
                    cex = 1
                    )
                ),
            x = 0.55,
            y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Grouped labels
create.barplot(
    filename = 'Barplot_Grouped_Labels.tiff',
    formula = values ~ samples,
    data = barplot.data,
    main = 'Grouped labels',
    xlab.lab = 'Samples',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Setting groups
    groups = mutation,
    col = default.colours(12, is.greyscale = FALSE)[11:12],
    # Grouped labels
    xaxis.lab = rep(c('nonsynonymous', 'other'), 15),
    group.labels = TRUE,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Stacked barplot
create.barplot(
    filename = 'Barplot_Stacked.tiff',
    formula = values ~ samples,
    data = barplot.data,
    main = 'Stacked bar chart',
    xlab.lab = 'Samples',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    groups = mutation,
    col = default.colours(12, is.greyscale = FALSE)[11:12],
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 2,
                        # reverse order to match stacked bar order
                        fill = rev(default.colours(12, is.greyscale = FALSE)[11:12])
                        ),
                    text = list(
                        # reverse order to match stacked bar order
                        lab = rev(c('Nonsynonymous SNV','Other SNV'))
                        ),
                    padding.text = 3,
                    cex = 1
                    )
                ),
            x = 0.55,
            y = 0.95
            )
        ),
    # Changing the plot from a grouped plot to a stacked plot
    stack = TRUE,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Panel organization
create.barplot(
    filename = 'Barplot_Panel_Layout.tiff',
    # Setting the panel layout
    formula = values ~ samples | mutation,
    data = barplot.data,
    main = 'Panel layout',
    xlab.lab = 'Samples',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Panel organization 2
create.barplot(
    filename = 'Barplot_Panel_Layout_2.tiff',
    formula = values ~ samples | mutation,
    data = barplot.data,
    main = 'Panel layout',
    xlab.lab = 'Samples',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Adjusting the panel layout
    layout = c(1,2),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Covariates
# Note: Covariates can also be created using the create.multiplot function

# set covariate colour schemes
covariate.colours.sex <- as.character(barplot.data$sex);
covariate.colours.sex[covariate.colours.sex == 'male'] <- 'dodgerblue';
covariate.colours.sex[covariate.colours.sex == 'female'] <- 'pink';

covariate.colours.stage <- as.character(barplot.data$stage);
covariate.colours.stage[covariate.colours.stage == 'I'] <- 'plum1';
covariate.colours.stage[covariate.colours.stage == 'II'] <- 'orchid1';
covariate.colours.stage[covariate.colours.stage == 'III'] <- 'orchid3';
covariate.colours.stage[covariate.colours.stage == 'IV'] <- 'orchid4';

covariate.colours.msi <- as.character(barplot.data$msi);
covariate.colours.msi[covariate.colours.msi == 'MSS'] <- 'chartreuse4';
covariate.colours.msi[covariate.colours.msi == 'MSI-High'] <- 'chartreuse2';

# create object to draw covariates
covariates.object <- list(
    rect = list(
        col = 'white',
        fill = covariate.colours.sex,
        lwd = 1.5
        ),
    rect = list(
        col = 'white',
        fill = covariate.colours.stage,
        lwd = 1.5
        ),
    rect = list(
        col = 'white',
        fill = covariate.colours.msi,
        lwd = 1.5
        )
    );

# see BoutrosLab.plotting.general::covariates.grob() for more information
covariate.object.grob <- covariates.grob(
    covariates = covariates.object,
    ord = c(1:15),
    side = 'top',
    size = 0.8
    );

# Create legend to explain covariates
covariates.legends <- list(
    legend = list(
        colours = c('dodgerblue','pink'),
        labels = c('male','female'),
        title = 'Sex',
        border = 'white'
        ),
    legend = list(
        colours = c('plum1', 'orchid1', 'orchid3', 'orchid4'),
        labels = c('I','II','III','IV'),
        title = 'Stage',
        border = 'white'
        ),
    legend = list(
        colours = c('chartreuse4','chartreuse2'),
        labels = c('MSS','MSI-High'),
        title = "MSI",
        border = 'white'
        )
    );

# see BoutrosLab.plotting.general::legend.grob() for more information 
covariate.legend.grob <- legend.grob(
    legends = covariates.legends,
    title.just = 'left'
    );

create.barplot(
    filename = 'Barplot_Covariates.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Covariates',
    ylab.lab = 'Mutations',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    yaxis.fontface = 1,
    # removing x-axis formatting to give space to covariates
    xaxis.tck = 0,
    xaxis.lab = rep('',15),
    # covariates
    legend = list(
        bottom = list(fun = covariate.object.grob),
        right = list(fun = covariate.legend.grob)
        ),
    key = list(
        x = 1,
        y = -0.015,
        text = list(
            lab = c('Sex','Stage','MSI')
            ),
        padding.text = 1
        ),
    bottom.padding = 4,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Horizontal orientation
create.barplot(
    filename = 'Barplot_Horizontal.tiff',
    # switch formula order
    formula = samples ~ values,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Plot horizontally',
    # Adjusting the panel layout
    plot.horizontal = TRUE,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Error bars
error.data <- data.frame(
    genes = rownames(microarray)[1:15],
    values = apply(microarray[1:15,1:58], 1, mean),
    error = apply(microarray[1:15,1:58], 1, sd)
    );

create.barplot(
    filename = 'Barplot_Error_Bars.tiff',
    # needs sequential x-axis
    formula = values ~ 1:15,
    data = error.data,
    y.error.up = error.data$error,
    xaxis.lab = error.data$genes,
    main = 'Error bars',
    xlab.lab = 'Gene',
    ylab.lab = 'Change in Expression',
    ylimits = c(0,14),
    yat = seq(0,14,2),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Grid lines
create.barplot(
    filename = 'Barplot_Gridlines.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Gridlines',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Grid lines
    draw.grid = TRUE,
    xgrid.at = seq(0,15,2),
	col = sex.colours,
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Grid lines 2
create.barplot(
    filename = 'Barplot_Gridlines_GreyBG.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Gridlines & grey background',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Grid lines
    background.col = 'grey85',
    draw.grid = TRUE,
    xgrid.at = seq(0,15,2),
    col = sex.colours,
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Labels
create.barplot(
    filename = 'Barplot_Labels.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Labels',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Labels
    text.above.bars = list(
        labels = c('*','27','15','*'), 
        padding = 0.75, 
        bar.locations = c(1, 3, 12, 14), 
        rotation = 0
        ),
    col = sex.colours,
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# lines
create.barplot(
    filename = 'Barplot_Lines.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Lines',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # Lines
    sample.order = 'increasing',
    line.func = function(x) {x = 10},
    line.from = 0,
    line.to = 16,
    line.col = 'darkgrey',
    col = sex.colours,
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Background rectangle
create.barplot(
    filename = 'Barplot_Bg_Rectangle.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Background rectangle',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    sample.order = 'increasing',
    # Background rectangle
    add.rectangle = TRUE,
    xleft.rectangle = seq(0.5, 14.5, 2),
    ybottom.rectangle = 0,
    xright.rectangle = seq(1.5, 15.5, 2),
    ytop.rectangle = 30,
    col.rectangle = 'lightgrey',
    col = sex.colours,
    legend = list(
        inside = list(
            fun = draw.key,
            args = list(
                key = list(
                    points = list(
                        col = 'black',
                        pch = 22,
                        cex = 3,
                        fill = c('dodgerblue', 'pink')
                        ),
                    text = list(
                        lab = c('Male','Female')
                        ),
                    padding.text = 5,
                    cex = 1
                    )
                ),
                # Positioning legend on plot
                x = 0.75,
                y = 0.95
            )
        ),
    description = 'Barplot created by BoutrosLab.plotting.general'
    );

# Raster
create.barplot(
    filename = 'Barplot_with_raster.tiff',
    formula = values ~ samples,
    data = barplot.data[barplot.data$mutation == 'nonsynonymous',],
    main = 'Raster fill',
    xlab.lab = 'Samples',
    ylab.lab = 'Nonsynonymous SNVs',
    ylimits = c(0,30),
    yat = seq(0,30,5),
    xaxis.cex = 1,
    yaxis.cex = 1,
    xlab.cex = 1.5,
    ylab.cex = 1.5,
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    # filling bars with raster
    raster = 1:10/10,
    raster.just = 'bottom',
    description = 'Description of image here'
    );

}
\keyword{hplot}
