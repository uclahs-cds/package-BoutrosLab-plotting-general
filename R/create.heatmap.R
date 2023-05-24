# The BoutrosLab.plotting.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO CREATE HEATMAPS ####################################################################
create.heatmap <- function(x, filename = NULL, clustering.method = 'diana', cluster.dimensions = 'both',
	rows.distance.method = 'correlation', cols.distance.method = 'correlation', cor.method = 'pearson',
	row.dendrogram = list(), col.dendrogram = list(), plot.dendrograms = 'both', force.clustering = FALSE,
	criteria.list = TRUE, covariates = list(), covariates.grid.row = NULL, covariates.grid.col = NULL,
	covariates.grid.border = NULL, covariates.row.lines = NULL, covariates.col.lines = NULL,
	covariates.reorder.grid.index = FALSE, covariates.padding = 0.25, covariates.top = list(),
	covariates.top.grid.row = NULL, covariates.top.grid.col = NULL, covariates.top.grid.border = NULL,
	covariates.top.row.lines = NULL, covariates.top.col.lines = NULL, covariates.top.reorder.grid.index = FALSE,
	covariates.top.padding = 0.25, covariate.legends = list(), legend.cex = 1, legend.title.cex = 1,
	legend.title.just = 'centre', legend.title.fontface = 'bold', legend.border = NULL, legend.border.padding = 1,
	legend.layout = NULL, legend.between.col = 1, legend.between.row = 1, legend.side = 'left',
	main = list(label = ''), main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3, right.size.add = 1,
	top.size.add = 1, right.dendrogram.size = 2.5, top.dendrogram.size = 2.5, scale.data = FALSE, yaxis.lab = NULL,
	xaxis.lab = NULL, xaxis.lab.top = NULL, xaxis.cex = 1.5, xaxis.top.cex = NULL,  yaxis.cex = 1.5, xlab.cex = 2,
	ylab.cex = 2, xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center',
	xlab.top.x = 0.5, xlab.top.y = 0, xat = TRUE, xat.top = NULL, yat = TRUE, xaxis.tck = NULL, xaxis.top.tck = NULL,
	yaxis.tck = NULL, xaxis.col = 'black', yaxis.col = 'black', col.pos = NULL, row.pos = NULL, cell.text = '',
	text.fontface = 1, text.cex = 1, text.col = 'black', text.position = NULL, text.offset = 0,
	text.use.grid.coordinates = TRUE, colourkey.cex = 3.6, xaxis.rot = 90, xaxis.rot.top = 90, yaxis.rot = 0,
	xlab.label = '', ylab.label = '', xlab.col = 'black', ylab.col = 'black', axes.lwd = 2, gridline.order = 'h',
	grid.row = FALSE, grid.col = FALSE, force.grid.row = FALSE, force.grid.col = FALSE, grid.limit = 50,
	row.lines = seq(0, ncol(x), 1) + 0.5, col.lines = seq(0, nrow(x), 1) + 0.5, colour.scheme = c(), total.colours = 99,
	colour.centering.value = 0, colour.alpha = 1, fill.colour = 'darkgray', at = NULL, print.colour.key = TRUE,
	colourkey.labels.at = NULL, colourkey.labels = NULL, top.padding = 0.1, bottom.padding = 0.5, right.padding = 0.5,
	left.padding = 0.5, x.alternating = 1, shrink = 1, row.colour = 'black', col.colour = 'black', row.lwd = 1, col.lwd = 1,
	grid.colour = NULL, grid.lwd = NULL, width = 6, height = 6, size.units = 'in', resolution = 1600,
	enable.warnings = FALSE, xaxis.covariates = NULL, xaxis.covariates.y = 0, yaxis.covariates = NULL,
	yaxis.covariates.x = NULL, description = 'Created with BoutrosLab.plotting.general', xaxis.fontface = 'bold',
	yaxis.fontface = 'bold', symbols = list(borders = NULL, squares = NULL, circles = NULL), same.as.matrix = FALSE,
	input.colours = FALSE, axis.xlab.padding = 0.1, stratified.clusters.rows = NULL, stratified.clusters.cols = NULL,
	inside.legend = NULL, style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE
    ) {

	### PARAMETER CHECKING #########################################################################
	if (preload.default == 'paper') {
		}
	else if (preload.default == 'web') {
		}

	# check that the resolution and size are sufficient for the dimensions of the data
	# using a conservative estimate that the heatmap is 50% of the plot
	# try to set default for xaxis.covariates.y value, if levelplot is resized user will have to input own value
	if (is.null(yaxis.covariates.x) && !is.null(yaxis.covariates)) {
		yaxis.covariates.x <- -0.037 * length(yaxis.covariates);
		}
	main <- unlist(main, use.names = FALSE);
	if (size.units == 'in') {
		if (nrow(x) > resolution * width * 0.5) {
			warning('HEATMAP: There are probably not enough pixels to represent all the columns in the heatmap. Try increasing the resolution or width.');
			}
		if (ncol(x) > resolution * height * 0.5) {
			warning('HEATMAP: There are probably not enough pixels to represent all the rows in the heatmap.  Try increasing the resolution or height.');
			}
		}

	# if you only have one column, you start to get weird behaviour, duplicating it fixes that.
	if (ncol(x) == 1) {
		x <- t(cbind(x, x));
		}
	# transpose matrix to keep original form
	if (same.as.matrix == TRUE) {
		x <- t(apply(x, 2, rev));
		for (i in c(1:(length(yaxis.lab) / 2))) {
			temp <- yaxis.lab[i];
			yaxis.lab[i] <- yaxis.lab[length(yaxis.lab) - i + 1];
			yaxis.lab[length(yaxis.lab) - i + 1] <- temp;
			}
		}

	# function to colour stratified dendrograms
	colbranches <- function(node, col) {
		# Find the attributes of current node
		node.attributes <- attributes(node);
		# Colour edges with requested colour
		attr(node, 'edgePar') <- c(node.attributes$edgePar, list(col = col, lwd = 2));
		return(node);
		}

	### CUSTOMIZE AXIS LABEL SIZES #################################################################
	data.directory <- system.file('optimal.heatmap.cex.txt', package = 'BoutrosLab.plotting.general');

	# check to see if the file was actually found
	if (!any(file.exists(data.directory))) {
		stop('Unable to find reference heatmap cex file (for x and y labels)');
		}

	# read in the listing of all reference xaxis.cex and yaxis.cex values
	xyaxis.ref.cex <- read.table(
		file = data.directory,
		header = TRUE,
		sep = '\t',
		row.names = NULL,
		as.is = TRUE
		);

	# vectorize all x-axis characteristics
	xaxis.cex <- rep(xaxis.cex, length.out = nrow(x));
	xaxis.rot <- rep(xaxis.rot, length.out = nrow(x));
	xaxis.col <- rep(xaxis.col, length.out = nrow(x));
	xaxis.rot[2] <- xaxis.rot.top;

	# vectorize all y-axis characteristics
	yaxis.cex <- rep(yaxis.cex, length.out = ncol(x));
	yaxis.col <- rep(yaxis.col, length.out = ncol(x));

	# see if we can map a customized xaxis label-size, but set a reasonable default
	xaxis.cex[is.na(xaxis.cex)] <- ifelse(
		test = nrow(x) < max(xyaxis.ref.cex$NumberOfRows),
		yes = xyaxis.ref.cex$optimal.cex[nrow(x)],
		no = 0
		);

	# see if we can map a customized xaxis label-size, but set a reasonable default
	yaxis.cex[is.na(yaxis.cex)] <- ifelse(
		test = ncol(x) < max(xyaxis.ref.cex$NumberOfRows),
		yes = xyaxis.ref.cex$optimal.cex[ncol(x)],
		no = 0
		);

	### SUBSET DATA ################################################################################
	# Extract a subset of data to work with
	x <- x[criteria.list, ];
	x <- as.matrix(x);
	if (TRUE == input.colours) {
		s <- unique(unlist(as.list(x)));
		for (i in c(1:length(s))) {
			x[x == s[i]] <- i;
			}
		storage.mode(x) <- 'numeric';
		total.colours <- length(s) + 1;
		colour.scheme <- s;
		}

	# Scale the data if necessary
	if (scale.data) {
		x <- t(x);
		x <- scale(x);
		x <- t(x);
		}

	# specifying top covariate
	if (length(covariates.top) > 0) {
		for (i in c(1:length(covariates.top))) {
			if (!is.null(covariates.top[[i]]$col) && covariates.top[[i]]$col != 'transparent') {
				break;
				}
			if (i == length(covariates.top)) {
				covariates.top.grid.border <- list(col = 'black', lwd = 2);
				}
			}
		}

	# Set default behaviour of x and y axes labels
	# NB: The rownames() and colnames() calls in the if() statements below are *correct*.
	#     Somehow the function is inverting what I think of as rows/columns (i.e. doing a t())
	if (1 == length(xaxis.lab)) { if (is.na(xaxis.lab)) { xaxis.lab <- rownames(x); } }
	if (1 == length(yaxis.lab)) { if (is.na(yaxis.lab)) { yaxis.lab <- colnames(x); } }

	### ERROR CHECKING FOR PARAMETERS #############################################################
	# error checking for column dendrogram
	if (length(col.dendrogram) > 0) {

		# ensure user did not specify both a dendrogram and a clustering method
		if (clustering.method != 'none' && cluster.dimensions %in% c('both', 'col', 'column', 'cols', 'columns')) {
			stop('Cannot provide a column dendrogram and also perform column-wise clustering');
			}
		# ensure dendrogram is correct class
		if (!is(col.dendrogram, 'dendrogram')) {
			stop('Invalid col.dendrogram parameter -- must be a dendrogram object');
			}
		# ensure dendrogram is correct size
		if (length(order.dendrogram(col.dendrogram)) != nrow(x)) {
			stop('Invalid col.dendrogram: should be of size ', nrow(x));
			}
		}

	# error checking for the row dendrogram
	if (length(row.dendrogram) > 0) {

		# ensure user did not specify both a dendrogram and a clustering method
		if (clustering.method != 'none' && cluster.dimensions %in% c('both', 'row', 'rows')) {
			stop('Cannot provide a row dendrogram and also perform row-wise clustering');
			}
		# ensure dendrogram is correct class
		if (!is(row.dendrogram, 'dendrogram')) {
			stop('Invalid row.dendrogram parameter -- must be a dendrogram object');
			}
		# ensure dendrogram is correct size
		if (length(order.dendrogram(row.dendrogram)) != ncol(x)) {
			stop('Invalid row.dendrogram: should be of size ', ncol(x));
			}
		}

	#error checking for input.colours = TRUE
	if (input.colours == TRUE) {

		if (clustering.method != 'none') {
			stop('Cannot cluster data if input.colours == TRUE');
			}
		if (!is.null(at)) {
			stop('at should not be specificed if input.colours == TRUE');
			}
		}

	### CLUSTERING & COVARIATES ###################################################################
	legend <- list();

	# Specify lattice settings for any images
	lattice.old.factor <- lattice.getOption('axis.padding')$factor;
	lattice.options('axis.padding' = list(factor = 0.5));

	# Include a column-based dendrogram if desired
	# Note: Because of the inversion of rows & columns, the heatmap's column-based dendrogram
	#       corresponds to the rows of the matrix
	dd.row.order <- NULL;
	dd.col.order <- NULL;

	if (length(col.dendrogram) > 0) {
		dd.row <- col.dendrogram;
		}
	else if (clustering.method != 'none' && cluster.dimensions %in% c('both', 'col', 'column', 'cols', 'columns')) {
		dd.row <- NULL;
		if (length(stratified.clusters.cols) > 0) {
			# loop through strata if stratified clusters have been specified
			for (i in c(1:length(stratified.clusters.cols))) {
				dd.row[[i]] <- BoutrosLab.plotting.general::create.dendrogram(
					x = x[stratified.clusters.cols[[i]], ],
					clustering.method = clustering.method,
					cluster.dimension = 'row',
					cor.method = cor.method,
					distance.method = cols.distance.method,
					force.clustering = force.clustering
					);
				}
			# calculate the new row order based on the clusters
			for (i in c(1:length(stratified.clusters.cols))) {
				dd.row.order <- c(dd.row.order, stratified.clusters.cols[[i]][order.dendrogram(dd.row[[i]])]);
				}
			# merge each dendrogram together
			dd.row <- do.call(merge, dd.row);
			# readjust all the rows/cols (they all want to be placed in the same spot)
			leafcount <- 0;
			stratacount <- 1;
			addition <- 0;
			dd.row <- dendrapply(
				dd.row,
				function(n) {
					if (is.leaf(n)) {
						n[1] <- n[1] + addition;
						leafcount <<- leafcount + 1;
						if (leafcount == length(stratified.clusters.cols[[stratacount]])) {
							addition <<- addition + length(stratified.clusters.cols[[stratacount]]);
							stratacount <<- stratacount + 1;
							leafcount <<- 0;
							}
						}
					return(n);
					}
				);
			}
		else {
			dd.row <- BoutrosLab.plotting.general::create.dendrogram(
				x = x,
				clustering.method = clustering.method,
				cluster.dimension = 'row',
				distance.method = cols.distance.method,
				cor.method = cor.method,
				force.clustering = force.clustering
				);
			}
		}

	if (exists('dd.row')) {
		# reorder data for plotting
		if (length(stratified.clusters.cols) > 0) {
			x <- x[dd.row.order, ];

			}
		else {
			x <- x[order.dendrogram(dd.row), ];
			}

		# reorder the cell.text
		if (length(cell.text) > 1) {
			col.pos <- match(col.pos, order.dendrogram(dd.row));
			}

		# reorder label characteristics to match clustering
		if (length(stratified.clusters.cols) > 0) {
			xaxis.lab <- xaxis.lab[dd.row.order];
			xaxis.cex <- xaxis.cex[dd.row.order];
			xaxis.rot <- xaxis.rot[dd.row.order];
			xaxis.col <- xaxis.col[dd.row.order];
			}
		else {
			xaxis.lab <- xaxis.lab[order.dendrogram(dd.row)];
			xaxis.cex <- xaxis.cex[order.dendrogram(dd.row)];
			xaxis.rot <- xaxis.rot[order.dendrogram(dd.row)];
			xaxis.col <- xaxis.col[order.dendrogram(dd.row)];
			}

		# if covariate bars are to be drawn, create them using the dendrogram ordering
		if (length(covariates.top) > 0) {
			covariates.top.grob <- BoutrosLab.plotting.general::covariates.grob(
				# reverse the covariates on the top dimension so the two match
				covariates = rev(covariates.top),
				ord = order.dendrogram(dd.row),
				side = 'top',
				size = top.size.add,
				grid.row = covariates.top.grid.row,
				grid.col = covariates.top.grid.col,
				grid.border = covariates.top.grid.border,
				row.lines = covariates.top.row.lines,
				col.lines = covariates.top.col.lines,
				reorder.grid.index = covariates.top.reorder.grid.index
				);
			}

		# create dendrogram grob if desired
		if (plot.dendrograms %in% c('both', 'top')) {
			dendrogram.top.grob <- latticeExtra::dendrogramGrob(
				x = dd.row,
				ord = order.dendrogram(dd.row),
				side = 'top',
				size = top.dendrogram.size,
				type = 'rectangle'
				);

			if (length(stratified.clusters.cols) > 0) {
				# this will remove the top of grob so that the unrelated dendrograms are not joined
				dendrogram.top.grob$children[[1]]$children[[1]]$y0 <-
				dendrogram.top.grob$children[[1]]$children[[1]]$y0[
					c( (1 + length(stratified.clusters.cols) * 2):length(dendrogram.top.grob$children[[1]]$children[[1]]$y0))
					];
				dendrogram.top.grob$children[[1]]$children[[1]]$y1 <-
				dendrogram.top.grob$children[[1]]$children[[1]]$y1[
					c( (1 + length(stratified.clusters.cols) * 2):length(dendrogram.top.grob$children[[1]]$children[[1]]$y1))
					];
				dendrogram.top.grob$children[[1]]$children[[1]]$x0 <-
				dendrogram.top.grob$children[[1]]$children[[1]]$x0[
					c( (1 + length(stratified.clusters.cols) * 2):length(dendrogram.top.grob$children[[1]]$children[[1]]$x0))
					];
				dendrogram.top.grob$children[[1]]$children[[1]]$x1 <-
				dendrogram.top.grob$children[[1]]$children[[1]]$x1[
					c( (1 + length(stratified.clusters.cols) * 2):length(dendrogram.top.grob$children[[1]]$children[[1]]$x1))
					];
				}
			}

		# if both covariates and dendrograms are to be drawn, place them in a grid and set the legend to hold the approntate grob
		if (length(covariates.top) > 0 && plot.dendrograms %in% c('both', 'top')) {

			top.layout <- grid.layout(
				nrow = 3,
				ncol = 1,
				widths = unit(1, 'null'),
				heights = unit(
					c(1, covariates.top.padding, 1),
					c('grobheight', 'lines', 'grobheight'),
					list(dendrogram.top.grob, NULL, covariates.top.grob)
					)
				);

			top.grob <- frameGrob(layout = top.layout);

			top.grob <- placeGrob(
				frame = top.grob,
				grob = dendrogram.top.grob,
				row = 1,
				col = 1
				);

			top.grob <- placeGrob(
				frame = top.grob,
				grob = covariates.top.grob,
				row = 3,
				col = 1
				);

			legend[['top']] <- list(fun = top.grob);
			}

		else if (length(covariates.top) > 0) {
			legend[['top']] <- list(fun = covariates.top.grob);
			}

		else if (plot.dendrograms %in% c('both', 'top')) {
			legend[['top']] <- list(fun = dendrogram.top.grob);
			}
		}

	# Include a row-based dendrogram if desired
	# Note: Because of the inversion of rows & columns, the heatmap's row-based dendrogram
	# corresponds to the columns of the matrix
	if (length(row.dendrogram) > 0) {
		dd.col <- row.dendrogram;
		}

	else if (clustering.method != 'none' && cluster.dimensions %in% c('both', 'row', 'rows')) {
		dd.col <- NULL;
		# loop through strata if stratified clusters have been specified
		if (length(stratified.clusters.rows) > 0) {
			for (i in c(1:length(stratified.clusters.rows))) {
				dd.col[[i]] <- BoutrosLab.plotting.general::create.dendrogram(
					x = x[, stratified.clusters.rows[[i]]],
					clustering.method = clustering.method,
					cluster.dimension = 'col',
					distance.method = rows.distance.method,
					cor.method = cor.method,
					force.clustering = force.clustering
					);
				}
			# calculate the new row order based on the clusters
			for (i in c(1:length(stratified.clusters.rows))) {
				dd.col.order <- c(dd.col.order, stratified.clusters.rows[[i]][order.dendrogram(dd.col[[i]])]);
				}
			# merge each dendrogram together
			dd.col <- do.call(merge, dd.col);
			# readjust all the rows/cols (they all want to be placed in the same spot)
			leafcount <- 0;
			stratacount <- 1;
			addition <- 0;
			dd.col <- dendrapply(
				dd.col,
				function(node) {
					if (is.leaf(node)) {
						node[1] <- node[1] + addition;
						leafcount <<- leafcount + 1;
						if (leafcount == length(stratified.clusters.rows[[stratacount]])) {
							addition <<- addition + length(stratified.clusters.rows[[stratacount]]);
							stratacount <<- stratacount + 1;
							leafcount <<- 0;
							}
						}
					return(node);
					}
				);
			}
		else {
			dd.col <- BoutrosLab.plotting.general::create.dendrogram(
				x = x,
				clustering.method = clustering.method,
				cluster.dimension = 'col',
				distance.method = rows.distance.method,
				cor.method = cor.method,
				force.clustering = force.clustering
				);
			}
		}

	if (exists('dd.col')) {

		# reorder data for plotting
		if (length(stratified.clusters.rows) > 0) {
			x <- x[, dd.col.order];
			}
		else {
			x <- x[, order.dendrogram(dd.col)];
			}
		# reorder the cell.text
		if (length(cell.text) > 1) {
			row.pos <- match(row.pos, order.dendrogram(dd.col));
			}

		# reorder label characteristics to match clustering
		if (length(stratified.clusters.rows) > 0) {
			yaxis.lab <- yaxis.lab[dd.col.order];
			yaxis.cex <- yaxis.cex[dd.col.order];
			yaxis.col <- yaxis.col[dd.col.order];
			}
		else {
			yaxis.lab <- yaxis.lab[order.dendrogram(dd.col)];
			yaxis.cex <- yaxis.cex[order.dendrogram(dd.col)];
			yaxis.col <- yaxis.col[order.dendrogram(dd.col)];
			}

		# if covariate bars are to be drawn, create them using the dendrogram ordering
		if (length(covariates) > 0) {
			covariates.right.grob <- BoutrosLab.plotting.general::covariates.grob(
				covariates = covariates,
				ord = order.dendrogram(dd.col),
				side = 'right',
				size = right.size.add,
				grid.row = covariates.grid.row,
				grid.col = covariates.grid.col,
				grid.border = covariates.grid.border,
				row.lines = covariates.row.lines,
				col.lines = covariates.col.lines,
				reorder.grid.index = covariates.reorder.grid.index
				);
			}

		# create dendrogram grob if desired
		if (plot.dendrograms %in% c('both', 'right')) {
			dendrogram.right.grob <- latticeExtra::dendrogramGrob(
				x = dd.col,
				ord = order.dendrogram(dd.col),
				side = 'right',
				size = right.dendrogram.size,
				type = 'rectangle'
				);
			if (length(stratified.clusters.rows) > 0) {
				# this will remove the top of the grob
				dendrogram.right.grob$children[[1]]$children[[1]]$y0 <-
				dendrogram.right.grob$children[[1]]$children[[1]]$y0[
					c( (1 + length(stratified.clusters.rows) * 2):length(dendrogram.right.grob$children[[1]]$children[[1]]$y0))
					];
				dendrogram.right.grob$children[[1]]$children[[1]]$y1 <-
				dendrogram.right.grob$children[[1]]$children[[1]]$y1[
					c( (1 + length(stratified.clusters.rows) * 2):length(dendrogram.right.grob$children[[1]]$children[[1]]$y1))
					];
				dendrogram.right.grob$children[[1]]$children[[1]]$x0 <-
				dendrogram.right.grob$children[[1]]$children[[1]]$x0[
					c( (1 + length(stratified.clusters.rows) * 2):length(dendrogram.right.grob$children[[1]]$children[[1]]$x0))
					];
				dendrogram.right.grob$children[[1]]$children[[1]]$x1 <-
				dendrogram.right.grob$children[[1]]$children[[1]]$x1[
					c( (1 + length(stratified.clusters.rows) * 2):length(dendrogram.right.grob$children[[1]]$children[[1]]$x1))
					];
				}
			}

		# if both covariates and dendrograms are to be drawn, place them in a grid
		# set the legend to hold the appropriate grob
		if (length(covariates) > 0 && plot.dendrograms %in% c('both', 'right')) {

			right.layout <- grid.layout(
				nrow = 1,
				ncol = 3,
				widths = unit(
					c(1, covariates.padding, 1),
					c('grobwidth', 'lines', 'grobwidth'),
					list(covariates.right.grob, NULL, dendrogram.right.grob)
					),
				heights = unit(1, 'null')
				);

			right.grob <- frameGrob(layout = right.layout);

			right.grob <- placeGrob(
				frame = right.grob,
				grob = covariates.right.grob,
				row = 1,
				col = 1
				);

			right.grob <- placeGrob(
				frame = right.grob,
				grob = dendrogram.right.grob,
				row = 1,
				col = 3
				);

			legend[['right']] <- list(fun = right.grob);
			}

		else if (length(covariates) > 0) {
			legend[['right']] <- list(fun = covariates.right.grob);
			}

		else if (plot.dendrograms %in% c('both', 'right')) {
			legend[['right']] <- list(fun = dendrogram.right.grob);
			}
		}

	# if dendrograms/clustering are not used but covariates given, draw covariate bars with default ordering
	if (!exists('dd.row') && length(covariates.top) > 0) {
		top.grob <- BoutrosLab.plotting.general::covariates.grob(
			# reverse the covariates on the top dimension so the two match
			covariates = rev(covariates.top),
			ord = c(1:nrow(x)),
			side = 'top',
			size = top.size.add,
			grid.row = covariates.top.grid.row,
			grid.col = covariates.top.grid.col,
			grid.border = covariates.top.grid.border,
			row.lines = covariates.top.row.lines,
			col.lines = covariates.top.col.lines,
			reorder.grid.index = covariates.top.reorder.grid.index
			);

		legend[['top']] <- list(fun = top.grob);
		}

	if (!exists('dd.col') && length(covariates) > 0) {
		right.grob <- BoutrosLab.plotting.general::covariates.grob(
			covariates = covariates,
			ord = c(1:ncol(x)),
			side = 'right',
			size = right.size.add,
			grid.row = covariates.grid.row,
			grid.col = covariates.grid.col,
			grid.border = covariates.grid.border,
			row.lines = covariates.row.lines,
			col.lines = covariates.col.lines,
			reorder.grid.index = covariates.reorder.grid.index
			);

		legend[['right']] <- list(fun = right.grob);
		}

	# draw covariate legends
	if (length(covariate.legends) > 0) {

		# get font family for grobPack which is different from what lattice accepts
		font.family <- 'sans';

		# create grob representing the legend
		if (is.null(legend.layout)) {
			legend.layout <- c(1, length(covariate.legends));
			}
		legend.grob.left <- NULL;
		legend.grob.top <- NULL;
		legend.grob.right <- NULL;
		if (length(legend.side) > 1 && length(covariate.legends[legend.side == 'left']) > 0) {
			legend.grob.left <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends[legend.side == 'left'],
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = legend.layout,
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}
		else if (length(legend.side) == 1 && legend.side == 'left') {
			legend.grob.left <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends,
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = legend.layout,
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}
		if (length(legend.side) > 1 && length(covariate.legends[legend.side == 'top']) > 0) {
			legend.grob.top <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends[legend.side == 'top'],
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = c(length(covariate.legends), 1),
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}
		else if (length(legend.side) == 1 && legend.side == 'top') {
			legend.grob.top <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends,
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = c(length(covariate.legends), 1),
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}
		if (length(legend.side) > 1 && length(covariate.legends[legend.side == 'right']) > 0) {
			legend.grob.right <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends[legend.side == 'right'],
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = legend.layout,
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}
		else if (length(legend.side) == 1 && legend.side == 'right') {
			legend.grob.right <- BoutrosLab.plotting.general::legend.grob(
				legends = covariate.legends,
				label.cex = legend.cex,
				title.cex = legend.title.cex,
				title.just = legend.title.just,
				title.fontface = legend.title.fontface,
				font.family = font.family,
				border = legend.border,
				border.padding = legend.border.padding,
				layout = legend.layout,
				between.col = legend.between.col,
				between.row = legend.between.row
				);
			}

		#legend.grob <- BoutrosLab.plotting.general::legend.grob(
		#	legends = covariate.legends,
		#	label.cex = legend.cex,
		#	title.cex = legend.title.cex,
		#	title.just = legend.title.just,
		#	title.fontface = legend.title.fontface,
		#	font.family = font.family,
		#	border = legend.border,
		#	border.padding = legend.border.padding,
		#	layout = legend.layout,
		#	between.col = legend.between.col,
		#	between.row = legend.between.row
		#	);

		# add the legend grob to the image
		if (!is.null(legend.grob.left)) {
			legend[['left']] <- list(fun = legend.grob.left);
			}
		if (!is.null(legend.grob.right)) {
			# check if we have already drawn something on the right side
			right.grob <- legend[['right']][['fun']];

			if (is.null(right.grob)) {
				legend[['right']] <- list(fun = legend.grob.right);
				}
			else {
				# NOTE: the convertUnit() call requires an open device
				# Check if any devices are open at this point
				# If not, the device created by the convertUnit() call
				# will be closed below
				devices.open <- FALSE;
				if (length(dev.list()) > 0) {
					devices.open <- TRUE;
					}
				# determine width of legend grob in cm
				legend.width.cm <- convertUnit(
					grobWidth(legend.grob.right),
					unitTo = 'cm',
					axisFrom = 'x',
					typeFrom = 'dimension',
					valueOnly = TRUE
					);

				# close the open device if it was opened for convertUnit()
				if (!devices.open) {
					dev.off();

					# remove the empty Rplots.pdf file if one was created (non-interactive)
					if (file.exists('Rplots.pdf')) {
						unlink('Rplots.pdf');
						}
					}

			# make a layout for the existing grob plus the legend
				right.layout.final <- grid.layout(
					nrow = 1,
					ncol = 2,
					widths = unit(
						x = c(1, legend.width.cm + 0.5),
						units = c('grobwidth', 'cm'),
						data = list(right.grob, NULL)
						),
					heights = unit(1, 'null'),
					respect = FALSE
					);

				# create a frame using this layout
				right.grob.final <- frameGrob(layout = right.layout.final);

				# place the existing grob
				right.grob.final <- placeGrob(
					frame = right.grob.final,
					grob = right.grob,
					row = 1,
					col = 1
					);

				# place the legend
				right.grob.final <- placeGrob(
					frame = right.grob.final,
					grob = legend.grob.right,
					row = 1,
					col = 2
					);

				legend[['right']] <- list(fun = right.grob.final);
				}
			}
		if (!is.null(legend.grob.top)) {
			# check if we have already drawn something on the top
			top.grob <- legend[['top']][['fun']];

			if (is.null(top.grob)) {
				legend[['top']] <- list(fun = legend.grob.top);
				}
			else {
				# NOTE: the convertUnit() call requires an open device
				# Check if any devices are open at this point
				# If not, the device created by the convertUnit() call
				# will be closed below
				devices.open <- FALSE;
				if (length(dev.list()) > 0) {
					devices.open <- TRUE;
					}
				# determine height of legend grob
				legend.height.cm <- convertUnit(
					grobHeight(legend.grob.top),
					unitTo = 'cm',
					axisFrom = 'y',
					typeFrom = 'dimension',
					valueOnly = TRUE
					);

				# close the open device if it was opened for convertUnit()
				if (!devices.open) {
					dev.off();

					# Remove the empty Rplots.pdf file if one was created (non-interactive)
					if (file.exists('Rplots.pdf')) {
						unlink('Rplots.pdf');
						}
					}

				# make a layout for the existing grob plus the legend
				top.layout.final <- grid.layout(
					ncol = 1,
					nrow = 2,
					heights = unit(
						x = c(legend.height.cm + 0.5, 1),
						units = c('cm', 'grobheight'),
						data = list(NULL, top.grob)
						),
					widths = unit(1, 'null'),
					respect = FALSE
					);

				# create a frame using this layout
				top.grob.final <- frameGrob(layout = top.layout.final);

				# place the existing grob
				top.grob.final <- placeGrob(
					frame = top.grob.final,
					grob = legend.grob.top,
					row = 1,
					col = 1
					);

				# place the legend
				top.grob.final <- placeGrob(
					frame = top.grob.final,
					grob = top.grob,
					row = 2,
					col = 1
					);

				legend[['top']] <- list(fun = top.grob.final);
				}
			}
		}
	legend[['inside']] <- inside.legend;
	# draw xaxis covariates
	xaxis.cov.height.cm <- 0;
	if (!is.null(xaxis.covariates)) {
		if (exists('dd.row')) {
			xaxis.covariate.grob <- BoutrosLab.plotting.general::covariates.grob(
				covariates = xaxis.covariates,
				ord = order.dendrogram(dd.row),
				side = 'top'
				);
			}
		else {
			xaxis.covariate.grob <- BoutrosLab.plotting.general::covariates.grob(
				covariates = xaxis.covariates,
				ord = c(1:nrow(x)),
				side = 'top'
				);
			}
		legend[['inside']] <- list(
			fun = xaxis.covariate.grob,
			x = 0.5,
			y = xaxis.covariates.y
			);
		xaxis.cov.height.cm <- convertUnit(
			grobHeight(xaxis.covariate.grob),
			unitTo = 'cm',
			axisFrom = 'y',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
		xaxis.cov.height.cm <- xaxis.cov.height.cm * 5;

		if (is.null(xaxis.lab)) {
			xaxis.lab <- rep(' ', nrow(x));
			}
		}

	# draw yaxis covariates
	yaxis.cov.height.cm <- 0;
	if (!is.null(yaxis.covariates)) {
		if (exists('dd.col')) {
			yaxis.covariate.grob <- BoutrosLab.plotting.general::covariates.grob(
				covariates = yaxis.covariates,
				ord = order.dendrogram(dd.col),
				side = 'right'
				);
			}
		else {
			yaxis.covariate.grob <- BoutrosLab.plotting.general::covariates.grob(
				covariates = yaxis.covariates,
				ord = c(1:ncol(x)),
				side = 'right'
				);
			}
		if (!is.null(xaxis.covariates)) {
			grob.layout <- grid.layout(1,1);
			grob.frame <- frameGrob(layout = grob.layout);
			legend[['inside']]$fun$framevp$x <- unit(legend[['inside']]$x, 'npc');
			legend[['inside']]$fun$framevp$y <- unit(legend[['inside']]$y - 0.0185 * length(xaxis.covariates), 'npc');
			grob.frame <- placeGrob(grob.frame,legend[['inside']]$fun);
			yaxis.covariate.grob$framevp$x <- unit(yaxis.covariates.x + 0.0185 * length(yaxis.covariates), 'npc');
			yaxis.covariate.grob$framevp$y <- unit(0.5, 'npc');
			grob.frame <- placeGrob(grob.frame,yaxis.covariate.grob);
			legend[['inside']]$fun <- grob.frame;
			legend[['inside']]$x <- 0.5;
			legend[['inside']]$y <- 0.5;
			}
		else {
			legend[['inside']] <- list(
				fun = yaxis.covariate.grob,
				x = yaxis.covariates.x,
				y = 0.5
				);
			}

		yaxis.cov.height.cm <- convertUnit(
			grobWidth(yaxis.covariate.grob),
			unitTo = 'cm',
			axisFrom = 'y',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);

		yaxis.cov.height.cm <- yaxis.cov.height.cm * 5;

		if (is.null(yaxis.lab)) {
			yaxis.lab <- rep(' ', ncol(x));
			}
		}

	# restore original lattice setting
	lattice.options('axis.padding' = list(factor = lattice.old.factor));

	### AUTOMATIC COLOUR-KEY HANDLING ##############################################################
	# work out data ranges, break point locations, and colour-number from input parameters
	if (is.null(at)) {
		min.value <- min(x - colour.centering.value, na.rm = TRUE);
		max.value <- max(x - colour.centering.value, na.rm = TRUE);
		at <- seq(from = min.value, to = max.value, length.out = total.colours);
		if (all(1 == at)) { # handle cases with a single colour/value of x
			at <- c(1:2);  # all values of x are 1. The 2 prevents an error, since duplicate values of at are not permitted
			# create the colour scheme
			my.palette <- c(colour.scheme);
			}
		}
	else {
		min.value <- min(at - colour.centering.value, na.rm = TRUE);
		max.value <- max(at - colour.centering.value, na.rm = TRUE);
		max.at <- max(at);
		min.at <- min(at);
		if (max(x, na.rm = TRUE) > max.at) {
			warning(
				paste(
					'max(x) =',
					max(x, na.rm = TRUE),
					'is greater than max(at) = ',
					max.at,
					'Clipped data will be plotted'
					)
				);
			x[x > max.at] <- max(at);
			}
		if (min(x, na.rm = TRUE) < min.at) {
			warning(
				paste(
					'min(x) =',
					min(x, na.rm = TRUE),
					'is greater than min(at) = ',
					min.at,
					'Clipped data will be plotted'
					)
				);
			x[x < min.at] <- min(at);
			}
		total.colours <- max(length(at), total.colours);
		}

	# determine whether the data is one-sided or two-sided
	is.twosided <- sign(min.value) != sign(max.value);

	# colour-handling: use a default colour scheme if one was not provided
	if (0 == length(colour.scheme)) {
		if (is.twosided) {
			colour.scheme <- c('blue', 'white', 'red');
			}
		else {
			colour.scheme <- c('white', 'red');
			}
		}

	# automatically approximate colour.alpha
	if (colour.alpha == 'automatic') {
		max.x <- max(as.numeric(x));
		min.x <- min(as.numeric(x));
		middle.x <- (max.x + min.x) / 2;
		difference <- max.x - min.x;
		upper.limit.count <- length(as.numeric(x)[as.numeric(x) > max.x - difference * 0.2]) +
		length(as.numeric(x)[as.numeric(x) < min.x + difference * 0.2]);
		lower.limit.count <- length(as.numeric(x)[as.numeric(x) > middle.x - difference * 0.2 & as.numeric(x) < middle.x + difference * 0.2]);
		ratio <- (lower.limit.count - upper.limit.count) / lower.limit.count;
		if (ratio < 0) {ratio <- ratio * lower.limit.count / upper.limit.count};
		colour.alpha <- 1 + ratio * 0.5;
		}

	# colour-handling: first handle legacy cases
	if (1 == length(colour.scheme) && FALSE == input.colours) {
		if (colour.scheme == 'RedWhiteBlue')	 { colour.scheme <- c('red', 'white', 'blue'); }
		else if (colour.scheme == 'WhiteBlack')      { colour.scheme <- c('white', 'black'); }
		else if (colour.scheme == 'BlueWhiteYellow') { colour.scheme <- c('blue', 'white', 'yellow'); }
		else { stop('Unknown colour scheme:', colour.scheme); }
		}

	# colour-handling: next cover one-sided colour schemes
	if (2 == length(colour.scheme)) {
		colour.function <- colorRamp(colour.scheme, space = 'Lab');
		my.palette <- rgb(colour.function(seq(0, 1, 1 / total.colours) ^ colour.alpha), maxColorValue = 255);
		}

	# colour-handling: then handle two-sided colour schemes
	if (3 == length(colour.scheme)) {

		# warn the user if they try to use a three-colour scheme with one-sided data
		if (!is.twosided) { warning('Using a three-colour scheme with one-sided data is not advised!'); }

		# create the colour scheme
		colour.function.low  <- colorRamp(colour.scheme[1:2], space = 'Lab');
		colour.function.high <- colorRamp(colour.scheme[2:3], space = 'Lab');

		# the number of negative colours is based on the fraction of the range that's below the center value
		# the number of positive colours is based on the number of negatives
		# leave one colour free for the center value
		neg.colours <- min.value / (max.value - min.value) * (total.colours - 1);
		neg.colours <- ceiling(abs(neg.colours));
		pos.colours <- total.colours - neg.colours - 1;

		# there is a potential for the colour allocation to go wrong when:
		#	1) we have one-sided data
		#	2) the colour-centering is at zero
		#	3) a three-colour scheme is requested
		# We try to automatically detect this case and provide a fix
		if (neg.colours < 1 | pos.colours < 1) {
			warning('Colour allocation scheme failed, moving to a default method');
			neg.colours <- round(total.colours / 2);
			pos.colours <- round(total.colours / 2);
			}

		# create the colour palette
		my.palette <- c(
			rgb( colour.function.low(seq(0, 1, 1 / neg.colours) ^ colour.alpha), maxColorValue = 255),
			colour.scheme[2], # this helps ensure that the values are centered properly
			rgb( colour.function.high(seq(0, 1, 1 / pos.colours) ^ (1 / colour.alpha)), maxColorValue = 255)
			);

		}

	# allow colour-schemes with > 3 colours
	if (3 < length(colour.scheme)) {

		# warn the user if they do this
		if (enable.warnings) { warning('Using a >three-colour scheme!'); }

		# only allow this behaviour with at-colour-type-handling
		if (is.null(at)) { stop('>3-colour schemes only work when at is specified'); }

		# create the colour scheme
		my.palette <- c(colour.scheme);

		}

	# colour-handling: lastly ensure that a palette was defined somehow
	if (!exists('my.palette')) {
		stop('Somehow no palette was ever defined');
		}

	# create the colour-key
	if (print.colour.key) {
		colour.key <- list(
			space = 'bottom',
			size = 1,
			width = 1.25,
			height = 1.0,
			labels = list(
				cex = colourkey.cex,
				at = colourkey.labels.at,
				labels = colourkey.labels
				),
			tick.number = 3
			);
		}
	else {
		colour.key <- FALSE;
		}

	### HEATMAP GENERATION #########################################################################
	if (is.null(xlab.top.label)) {

		# if x-labels are on top, put xlabel on top too
		if (2 == x.alternating) {
			xlab.top.label <- xlab.label;
			xlab.label <- '';
			}
		}

	# determine length of tck marks
	# tick lengths depend on xaxis.tck and x.alternating
	x.tck <- c(0, 0);
	if (x.alternating > 0) {
		if (!is.null(xaxis.tck)) {
			if (1 == x.alternating) {
				x.tck <- c(xaxis.tck, 0);
				}
			else if (2 == x.alternating) {
				x.tck <- c(0, xaxis.tck);
				}
			else {
				# special case to print on top and below
				x.tck <- c(xaxis.tck, xaxis.tck);
				}
			}
		else if (nrow(x) < 65) {
			if (1 == x.alternating) {
				x.tck <- c(0.2 + xaxis.cov.height.cm, 0.2);
				}
			else if (2 == x.alternating) {
				x.tck <- c(0.2, xaxis.cov.height.cm);
				}
			else {
				# special case to print on top and below
				x.tck <- c(0.2 + xaxis.cov.height.cm, 0.2 + xaxis.cov.height.cm);
				}
			}
		else {
			if (1 == x.alternating) {
				x.tck <- c(0 + xaxis.cov.height.cm, 0);
				}
			else if (2 == x.alternating) {
				x.tck <- c(0, xaxis.cov.height.cm);
				}
			# special case to print on top and below
			else {
				x.tck <- c(0 + xaxis.cov.height.cm, 0 + xaxis.cov.height.cm);
				}
			}
		}

	if (!is.null(grid.colour)) {
		row.colour <- grid.colour;
		col.colour <- grid.colour;
		cat(paste0('CAUTION: grid.colour is DEPRECATED!  Use row.colour/col.colour. Using: ', grid.colour, '\n'));
		}
	if (!is.null(grid.lwd)) {
		row.lwd <- grid.lwd;
		col.lwd <- grid.lwd;
		cat(paste0('CAUTION: grid.lwd is DEPRECATED!  Use row.lwd/col.lwd. Using: ', grid.lwd, '\n'));
		}

	# function to draw different top and bottom axes
	xscale.components.new <- function(...) {
		args <- xscale.components.default(...);
		args$top <- args$bottom;

		if (length(xat.top) == 0) {
			xat.top <- c(1:length(xaxis.lab.top));
			}

		args$top$ticks$at <- xat.top;
		args$top$labels$at <- xat.top;
		args$top$labels$labels <- xaxis.lab.top;

		return(args);
		}
	if (is.null(xaxis.top.cex)) {xaxis.top.cex <- xaxis.cex;}

	# look at nrow and ncol and if exceed limit (for now, default limit = 50), turn off grid lines
	if ( (ncol(x) > grid.limit & grid.row == TRUE) & force.grid.row != TRUE) {
		grid.row <- FALSE;
		cat(paste0('Warning: number of rows exceeded limit (', grid.limit, '), row lines are turned off.
				Please set "force.grid.row" to TRUE to override this\n'));
		}
	if ( (nrow(x) > grid.limit & grid.col == TRUE) & force.grid.col != TRUE) {
		grid.col <- FALSE;
		cat(paste0('Warning: number of colum ns exceeded limit (', grid.limit, '), column lines are turned off.
				Please set "force.grid.col" to TRUE to override this\n'));
		}

	# create heatmap
	trellis.object <- lattice::levelplot(
		x,
		panel = function(...) {

			panel.fill(col = fill.colour);
			panel.levelplot(...);
			if (text.use.grid.coordinates) {
				panel.text(col.pos, row.pos, cell.text, font = text.fontface, cex = text.cex, col = text.col, pos = text.position, offset = text.offset);
				}
			else {
				for (i in 1:length(cell.text)) {
					grid.text(
						x = unit(text.position[[i]][1], 'npc'),
						y = unit(text.position[[i]][2], 'npc'),
						label = cell.text[i],
						gp = gpar(col = text.col, cex = text.cex, fontface = text.fontface));
					}
				}
			positions <- NULL;
			colours <- NULL;
			bordercolours <- 'white';
			sizes <- NULL;
			xright <- NULL;
			xleft <- NULL;
			ytop <- NULL;
			ybottom <- NULL;

			if (with(symbols, !is.null(symbols$borders) || !is.null(symbols$circles) || !is.null(symbols$squares))) {

				# divide up the three types of symbols
				borders <- symbols$borders;
				squares <- symbols$squares;
				circles <- symbols$circles;

				# add borders if requested
				if (!is.null(borders)) {
					for (i in 1:length(borders)) {
						if (3 == length(borders[[i]])) {
							# find positions on heatmap where the symbols should be drawn
							if (same.as.matrix == TRUE) {
								borders[[i]]$x <- t(apply(borders[[i]]$x, 2, rev));
								}
							positions[[i]] <- which(borders[[i]]$x);

							# gather all the necessary colours from those positions in the colour matrix
							bordercolours <- c(bordercolours, as.vector(borders[[i]]$col[positions[[i]]]));

							# gather all the necessary sizes from those positions in the size matrix
							sizes <- c(sizes, as.vector(borders[[i]]$size[positions[[i]]]));

							# calculate the actual positions needed for each coordinate on the heatmap
							xright <- c(xright, positions[[i]] %% nrow(borders[[i]]$x));
							xright <- replace(xright, xright == 0, nrow(borders[[i]]$x));
							xleft <- c(xleft, positions[[i]] %% nrow(borders[[i]]$x));
							xleft <- replace(xleft, xleft == 0, nrow(borders[[i]]$x));
							ytop <- c(ytop, (positions[[i]] - 0.01) %/% nrow(borders[[i]]$x) + 1);
							ybottom <- c(ybottom, (positions[[i]] - 0.01) %/% nrow(borders[[i]]$x) + 1);
							colours <- c(colours, rep('transparent', length(positions[[i]])));
							}
						else {
							if (same.as.matrix == TRUE) {
								templeft <- borders[[i]]$xleft;
								tempright <- borders[[i]]$xright;
								borders[[i]]$xleft <- borders[[i]]$ybottom;
								borders[[i]]$xright <- borders[[i]]$ytop;
								borders[[i]]$ybottom <- ncol(x) - tempright + 1;
								borders[[i]]$ytop <- ncol(x) - templeft + 1;
								}
							xright <- c(xright, borders[[i]]$xright);
							xleft <- c(xleft, borders[[i]]$xleft);
							ytop <- c(ytop, borders[[i]]$ytop);
							ybottom <- c(ybottom, borders[[i]]$ybottom);
							sizes <- c(sizes, borders[[i]]$size);
							bordercolours <- c(bordercolours, borders[[i]]$col);
							colours <- c(colours, 'transparent');
							}
						}

					# adjust coordinates to be at edge of cells
					ybottom <- ybottom - 0.5;
					xleft <- xleft - 0.5;
					ytop <- ytop + 0.5;
					xright <- xright + 0.5;
					}

				# add squares if requested
				if (!is.null(squares)) {
					for (i in 1:length(squares)) {
						if (same.as.matrix == TRUE) {
							squares[[i]]$x <- t(apply(squares[[i]]$x, 2, rev));
							}

						positions[[i]] <- which(squares[[i]]$x);
						bordercolours <- c(bordercolours, rep('transparent', length(positions[[i]])));
						sizes <- c(sizes, as.vector(squares[[i]]$size[positions[[i]]]));
						xright <- c(xright, positions[[i]] %% nrow(squares[[i]]$x));
						xright <- replace(xright, xright == 0, nrow(squares[[i]]$x));
						xleft <- c(xleft, positions[[i]] %% nrow(squares[[i]]$x));
						xleft <- replace(xleft, xleft == 0, nrow(squares[[i]]$x));
						ytop <- c(ytop, (positions[[i]] - 0.01) %/% nrow(squares[[i]]$x) + 1);
						ybottom <- c(ybottom, (positions[[i]] - 0.01) %/% nrow(squares[[i]]$x) + 1);
						colours <- c(colours, as.vector(squares[[i]]$col[positions[[i]]]));
						}
					}

				if (!is.null(xright)) {
					panel.rect(
						xleft = xleft + (sizes * 0.01),
						ybottom = ybottom + (sizes * 0.01),
						ytop = ytop - (sizes * 0.01),
						xright = xright - (sizes * 0.01),
						col = colours,
						alpha = 1,
						border = bordercolours,
						lwd = sizes
						);
					}
				xright <- NULL;
				ytop <- NULL;
				colours <- NULL;
				sizes <- NULL;

				# add circles if requested
				if (!is.null(circles)) {
					for (i in 1:length(circles)) {
						if (same.as.matrix == TRUE) {
							circles[[i]]$x <- t(apply(circles[[i]]$x, 2, rev));
							}

						positions[[i]] <- which(circles[[i]]$x);
						sizes <- c(sizes, as.vector(circles[[i]]$size[positions[[i]]]));
						xright <- c(xright, positions[[i]] %% nrow(circles[[i]]$x));
						xright <- replace(xright, xright == 0, nrow(circles[[i]]$x));
						ytop <- c(ytop, (positions[[i]] - 0.01) %/% nrow(circles[[i]]$x) + 1);
						colours <- c(colours, as.vector(circles[[i]]$col[positions[[i]]]));
						}
					}

				if (!is.null(xright)) {
					grid.circle(
						x = xright,
						y = ytop,
						r = sizes * 0.01,
						draw = TRUE,
						default.units = 'native',
						gp = gpar(fill = colours, col = 'transparent')
						);
					}
				}

			# draw grid lines if requested
			if ('h' == gridline.order) {
				if (grid.row) {
					panel.abline(
						h = row.lines,
						v = 0,
						col.line = row.colour,
						lwd = row.lwd
						);
					}
				if (grid.col) {
					panel.abline(
						h = 0,
						v = col.lines,
						col.line = col.colour,
						lwd = col.lwd
						);
					}
				}
			else if ('v' == gridline.order) {
				if (grid.col) {
					panel.abline(
						h = 0,
						v = col.lines,
						col.line = col.colour,
						lwd = col.lwd
						);
					}
				if (grid.row) {
					panel.abline(
						h = row.lines,
						v = 0,
						col.line = row.colour,
						lwd = row.lwd
						);
					}
				}
			},
		aspect = 'fill',
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					col = xaxis.col,
					tck = x.tck,
					alternating = x.alternating,
					rot = xaxis.rot,
					at = xat,
					fontface = if ('Nature' == style) {'plain'} else (xaxis.fontface)
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					rot = yaxis.rot,
					tck = if (! is.null(yaxis.tck)) {
						c(yaxis.tck, 0);
						}
					else if (ncol(x) < 65) {
						c(0.2 + yaxis.cov.height.cm, 0.2)
						}
					else {
						c(0 + yaxis.cov.height.cm, 0)
						},
					axs = 'r',
					alternating = 1,
					at = yat,
					fontface = if ('Nature' == style) {'plain'} else (yaxis.fontface)
					)
				)
			),
		xscale.components = xscale.components.new,
	#	axis = axis.CF,
		col.regions = my.palette,
		colorkey = colour.key,
		legend = legend,
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (main == '') { 0.1 } else { 1.0 },
				main.key.padding = if ('' == main) { 0.1 } else { 1.0 },
				key.top = 1,
				key.axis.padding = if (ncol(x) < 30) { 0.9 } else { 0.5 },
				axis.top = if (1 == x.alternating) { 0.1 } else { 1.0 },
				axis.bottom = if (is.null(xaxis.lab)) { 0.2 } else { 1.0 },
				axis.xlab.padding = axis.xlab.padding,
				xlab = if (!is.expression(xlab.label) && '' == xlab.label) { 0.1 } else { 1 },
				xlab.key.padding = 0.5,
				key.bottom = 1,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 1,
				key.ylab.padding = if ( (!is.expression(ylab.label) && '' == ylab.label) || 0 == length(covariate.legends)) { 0.1 } else { 1.0 },
				ylab = if (!is.expression(ylab.label) && '' == ylab.label) { 0.1 } else { 1 },
				ylab.axis.padding = 0.1,
				axis.left = 1,
				axis.right = 0.1,
				axis.key.padding = if (nrow(x) < 30) { 0.9 } else { 0.5 },
				key.right = 1,
				right.padding = right.padding
				),
			par.xlab.text = list(
				cex = xaxis.cex,
				lineheight = xaxis.cex
				),
			par.ylab.text = list(
				cex = yaxis.cex,
				lineheight = yaxis.cex
				)
			),
		lattice.options = list(
			axis.padding = list(
				factor = 0.5
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = if ('Nature' == style) {'plain'} else ('bold')
				)
			),
		xlab.top = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.top.label,
				cex = xlab.top.cex,
				col = xlab.top.col,
				fontface = if ('Nature' == style) {'plain'} else {'bold'},
				just = xlab.top.just,
				x = xlab.top.x,
				y = xlab.top.y
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = if ('Nature' == style) {'plain'} else ('bold')
				)
			),
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				cex = main.cex,
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				just = main.just,
				x = main.x
				)
			),
		shrink = shrink,
		pretty = TRUE,
		at = at
		);

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Ensure sufficient resolution for graphs
		if (resolution < 1200) {
			resolution <- 1200;
			warning('Setting resolution to 1200 dpi.');
			}

		# Other required changes which are not accomplished here
		warning('Nature also requires italicized single-letter variables and en-dashes
			for ranges and negatives. See example in documentation for how to do this.');

		warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend');
		}

	else if ('BoutrosLab' == style) {
		# Nothing happens
		}

	else {
		warning("The style parameter only accepts 'Nature' or 'BoutrosLab'.");
		}

	# output the object
	return(
		BoutrosLab.plotting.general::write.plot(
			trellis.object = trellis.object,
			filename = filename,
			height = height,
			width = width,
			size.units = size.units,
			resolution = resolution,
			enable.warnings = enable.warnings,
			description = description
			)
		);
	}
