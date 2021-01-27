\name{covariates.grob}
\alias{covariates.grob}
\title{Create one or more covariate bars}
\description{Takes a list of covariate bar annotates and creates a grid graphical object for them}
\usage{
covariates.grob(
	covariates,
	ord,
	side = 'right',
	size = 1,
	grid.row = NULL,
	grid.col = NULL,
	grid.border = NULL,
	row.lines = NULL,
	col.lines = NULL,
	reorder.grid.index = FALSE,
        x = 0.5,
        y = 0.5
	);
}
\arguments{
    \item{covariates}{Any covariate annotate to add to the plot, as a fully formed list.}
    \item{ord}{A vector of integer indices indicating the order of the items in the covariate bars.}
    \item{side}{Intended position of the covariate bar when added as a legend. Allowed positions are \dQuote{right} and \dQuote{top}.}
    \item{size}{The size of each covariate bar in units of \dQuote{lines}.}
    \item{grid.row}{A list of parameters to be passed to \code{gpar} specifying the behaviour of row lines in the covariate bars. See \code{Notes} for details.}
    \item{grid.col}{A list of parameters to be passed to \code{gpar} specifying the behaviour of column lines in the covariate bars.}
    \item{grid.border}{A list of parameters to be passed to \code{gpar} specifying the behaviour of the border around the covariate bars.}
    \item{row.lines}{Vector of row indices where grid lines should be drawn. If NULL (default), all row lines are drawn. Ignored if \code{grid.row} is not specified.}
    \item{col.lines}{Vector of column indices where grid lines should be drawn. If NULL (default), all column lines are drawn. Ignored if \code{grid.col} is not specified.}
    \item{reorder.grid.index}{Boolean specifying whether grid line indices should be re-ordered according to the \code{ord} argument. Defaults to FALSE.}
    \item{x}{x coordinate in npc coordinate system}
    \item{y}{y coordinate in npc coordinate system} 
}
\value{A grid graphical object (grob) representing the covariate bar(s)}
\author{Lauren Chong}
\seealso{\code{\link{gpar}}}
\section{Notes}{
This code is an adaptation of the \code{dendrogramGrob} function in the \code{latticeExtra} package. It uses functions of the \code{grid} package.

By default, the covariate bar grid is drawn via borders around individual rectangles using the parameters specified in the \code{covariates} argument (col, lwd, etc.). If \code{grid.row}, \code{grid.col}, or \code{grid.border} are specified by the user, additional grid lines are drawn over any existing ones using the parameters in these lists.
}
\examples{
# The 'cairo' graphics is preferred but on M1 Macs this is not available
bitmap.type = getOption('bitmapType')
if (capabilities('cairo')) {
	bitmap.type <- 'cairo';
	}

# create temp data
set.seed(1234567890);

x <- outer(-5:5, -5:5, '*') + matrix(nrow = 11, ncol = 11, data = runif(11 * 11));
colnames(x) <- paste('col', 1:11, sep = '-');
rownames(x) <- paste('row', 1:11, sep = '-');

# set covariates
covariate.colours1 <- x[,1]
covariate.colours1[covariate.colours1 >= 0] <- default.colours(3)[1];
covariate.colours1[covariate.colours1 != default.colours(3)[1]] <- default.colours(3)[2];

covariate.colours2 <- x[,1]
covariate.colours2[covariate.colours2 >= 0] <- default.colours(3)[2];
covariate.colours2[covariate.colours2 != default.colours(3)[2]] <- default.colours(3)[3];

# create an object to draw the covariates from
covariates1 <- list(
    rect = list(
        col = 'black',
        fill = covariate.colours1,
        lwd = 1.5
        ),
    rect = list(
        col = 'black',
        fill = covariate.colours2,
        lwd = 1.5
        )
    );

# create a covariates grob using a simple incremental ordering and default behaviour
covariates.grob1 <- covariates.grob(
    covariates = covariates1,
    ord = c(1:ncol(x)),
    side = 'right'
    );
tiff(
    filename = tempfile(pattern = 'covariates_grob1', fileext = '.tiff'), 
    type = bitmap.type, 
    width = 6, 
    height = 6, 
    units = 'in', 
    res = 1000, 
    compression = 'lzw'
    );
grid.draw(covariates.grob1);
dev.off();

# create a dendrogram for x
cov.dendrogram <- BoutrosLab.plotting.general::create.dendrogram(
    x = x,
    clustering.method = 'average'
    );

covariates2 <-list(
    rect = list(
        col = 'black',
        fill = covariate.colours2,
        lwd = 1.5
        )
    );

# create a covariates grob using the dendrogram ordering and double the default size
covariates.grob2 <- covariates.grob(
    covariates = covariates2,
    ord = order.dendrogram(cov.dendrogram),
    side = 'top',
    size = 2
    );
tiff(
    filename = tempfile(pattern = 'covariates_grob2', fileext = '.tiff'), 
    type = bitmap.type, 
    width = 6, 
    height = 6, 
    units = 'in', 
    res = 1000, 
    compression = 'lzw'
    );
grid.draw(covariates.grob2);
dev.off();

# add a border of a different colour
covariates.grob3 <- covariates.grob(
    covariates = covariates1,
    ord = c(1:ncol(x)),
    side = 'right',
    grid.border = list(col = 'red', lwd = 1.5)
    );
tiff(
    filename = tempfile(pattern = 'covariates_grob3', fileext = '.tiff'), 
    type = bitmap.type, 
    width = 6, 
    height = 6, 
    units = 'in', 
    res = 1000, 
    compression = 'lzw'
    );
grid.draw(covariates.grob3);
dev.off();

# create covariates with transparent rectangle borders
covariates3 <- list(
    rect = list(
        col = 'transparent',
        fill = covariate.colours1,
        lwd = 1.5
        ),
    rect = list(
        col = 'transparent',
        fill = covariate.colours2,
        lwd = 1.5
        )
    );

# add column grid lines and a border with default gpar settings
covariates.grob4 <- covariates.grob(
    covariates = covariates3,
    ord = c(1:nrow(x)),
    side = 'top',
    grid.col = list(col = 'black', lty = 3),
    grid.border = list()
    );
tiff(
    filename = tempfile(pattern = 'covariates_grob4', fileext = '.tiff'), 
    type = bitmap.type, 
    width = 6, 
    height = 6, 
    units = 'in', 
    res = 1000, 
    compression = 'lzw'
    );
grid.draw(covariates.grob4);
dev.off();

# draw a subset of row/column lines
covariates.grob5 <- covariates.grob(
    covariates = covariates3,
    ord = order.dendrogram(cov.dendrogram),
    side = 'right',
    grid.row = list(lineend = 'butt', lwd = 2),
    row.lines = 6,
    reorder.grid.index = FALSE, # note: this is already set by default
    grid.col = list(lty = 2),
    col.lines = c(0,1)
    );
tiff(
    filename = tempfile(pattern = 'covariates_grob5', fileext = '.tiff'),
    type = bitmap.type, 
    width = 6, 
    height = 6, 
    units = 'in', 
    res = 1000, 
    compression = 'lzw'
    );
grid.draw(covariates.grob5);
dev.off();
}
