# The BoutrosLab.plotting.general package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO CREATE UPSETPLOTS #################################################################
create.upsetplot <- function(
	# upset plot specific parameters
	x, force.unique = TRUE, hierarchical = FALSE, sorting = 'frequency', sorting.reverse = FALSE, set.order = NULL, membership.order = NULL, set.sorting.reverse = FALSE,
	include.set.barplot = TRUE, return.list = FALSE, include.zeros = TRUE, minimum.intersection.size = NULL, minimum.degree = NULL, minimum.set.size = NULL,
	# plot.horizontal = FALSE,
	# intersection barplot
	ylab.label = 'Intersection Size', ylab.cex = 2, ylab.col = 'black',
	yaxis.lab = TRUE, yaxis.col = 'black', yaxis.fontface = 'bold', yaxis.cex = 1.5, yaxis.rot = 0, yaxis.tck = c(1, 0),
	ylimits = NULL, yat = TRUE,
	xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0,
	int.abline.h = NULL, int.abline.v = NULL, int.abline.lty = 1, int.abline.lwd = NULL, int.abline.col = 'black',
	int.col = 'black', int.border.col = 'black', int.border.lwd = 1, int.background.col = 'transparent', int.box.ratio = 2, int.reference = FALSE,
	int.add.grid = FALSE, int.xgrid.at = TRUE, int.ygrid.at = yat, int.grid.lwd = 5, int.grid.col = NULL,
	int.add.text = FALSE, int.text.labels = NULL, int.text.x = NULL, int.text.y = NULL, int.text.col = 'black', int.text.cex = 1,
	int.text.fontface = 'bold', int.add.rectangle = FALSE, int.xleft.rectangle = NULL, int.ybottom.rectangle = NULL, int.xright.rectangle = NULL,
	int.ytop.rectangle = NULL, int.col.rectangle = 'grey85', int.alpha.rectangle = 1,
	int.text.above.bars = list(labels = NULL, padding = NULL, bar.locations = NULL, rotation = 0),
	# set barplot
	xlab.label = 'Set Size', xlab.cex = 2, xlab.col = 'black',
	xaxis.lab = TRUE, xaxis.col = 'black', xaxis.fontface = 'bold', xaxis.cex = 1.5, xaxis.rot = 0, xaxis.tck = c(1, 0),
	xlimits = NULL, xat = TRUE,
	set.abline.h = NULL, set.abline.v = NULL, set.abline.lty = 1, set.abline.lwd = NULL, set.abline.col = 'black',
	set.col = 'black', set.border.col = 'black', set.border.lwd = 1, set.background.col = 'transparent', set.box.ratio = 2, set.reference = FALSE,
	set.add.grid = FALSE, set.xgrid.at = xat, set.ygrid.at = TRUE, set.grid.lwd = 5, set.grid.col = NULL,
	set.add.text = FALSE, set.text.labels = NULL, set.text.x = NULL, set.text.y = NULL, set.text.col = 'black', set.text.cex = 1,
	set.text.fontface = 'bold', set.add.rectangle = FALSE, set.xleft.rectangle = NULL, set.ybottom.rectangle = NULL, set.xright.rectangle = NULL,
	set.ytop.rectangle = NULL, set.col.rectangle = 'grey85', set.alpha.rectangle = 1,
	set.text.above.bars = list(labels = NULL, padding = NULL, bar.locations = NULL, rotation = 0),
	# membership scatterplot
	dot.fill.col = 'grey70', dot.col = 'black', dot.add.grid = FALSE, dot.grid.colour = 'grey85',
	dot.fill.col.border = 'grey70', dot.col.border = 'black', axes.lty = 'dashed', add.axes = FALSE,
	dot.yaxis.col = 'black', dot.yaxis.fontface = 'bold', dot.yaxis.cex = 1.5, dot.yaxis.rot = 0, dot.yaxis.tck = c(1, 0),
	cex = 5, pch = 19, alpha = 1, line.col = 'black', line.lwd = 10,
	dot.abline.h = NULL, dot.abline.v = NULL, dot.abline.lty = 1, dot.abline.lwd = NULL, dot.abline.col = 'black',
	dot.add.rectangle = FALSE, dot.xleft.rectangle = NULL, dot.ybottom.rectangle = NULL, dot.xright.rectangle = NULL,
	dot.ytop.rectangle = NULL, dot.col.rectangle = 'grey85', dot.alpha.rectangle = 1,
	dot.background.rectangles = 'row',
	# shared by all
	axes.lwd = 1, strip.col = 'white', strip.cex = 1, strip.fontface = 'bold',
	raster = NULL, raster.vert = TRUE, raster.just = 'center', raster.width.dim = unit(2 / 37, 'npc'),
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	use.legacy.settings = FALSE, enable.warnings = FALSE,
	resolution = 1600, ylab.axis.padding = 0.5, xlab.axis.padding = 0.5,
	# multipanel plot
	plot.objects.heights = c(2, 1), plot.objects.widths = c(2, 1),
	filename = NULL, main = '', main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3,
	x.spacing = 0, y.spacing = 0,
	legend = NULL, left.legend.padding = 0, right.legend.padding = 0, bottom.legend.padding = 0, top.legend.padding = 0,
	top.padding = 0.5, bottom.padding = 0.5, right.padding = 0.5, left.padding = 0.5,
	height = 10, width = 10, size.units = 'in'
	) {

	### store data on mount
        tryCatch({
			dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
                        if (!dir.exists(dir.name)) {
                                dir.create(dir.name);
                                }
			funcname <- 'create.upsetplot';
                        print.to.file(dir.name, funcname, data, filename);
                        },
                warning = function(w) {
                        },
                error = function(e) {
                	});

	### THIS IS A WRAPPER TO CREATE AN ENTIRE OR COMPONENTS OF UPSET PLOTS
	### FOR FURTHER CUSTOMIZATION, PLEASE CREATE EACH COMPONENT SEPARATELY

	### set information from x

	if (!is.null(minimum.set.size)){
		x <- x[sapply(x, length) >= minimum.set.size]
	}

	x.partitions <- VennDiagram::get.venn.partitions(
						x = x,
						force.unique = force.unique,
						hierarchical = hierarchical,
						# for now, not interested in the partitions of x
						keep.elements = FALSE);
	if (is.null(set.order)){
		set.order <- names(sort(-sapply(x, length)))
		if (set.sorting.reverse){
			set.order <- names(sort(sapply(x, length)))
			}
		}

	### order by set names of x
	# will give an error is the set names given in set.order is not in x
	x.partitions <- x.partitions[, c(set.order, '..set..', '..count..')];
	x.partitions.reverse <- as.data.frame(!x.partitions[, set.order]);
	x.partitions <- x.partitions[do.call(order, x.partitions.reverse), ];
	x.partitions[, '..degree..'] <- rowSums(x.partitions[, set.order]);

	if (!include.zeros){
		x.partitions <- x.partitions[x.partitions$..count.. > 0, ]
	}

	if (!is.null(minimum.intersection.size)){
		x.partitions <- x.partitions[x.partitions$..count.. >= minimum.intersection.size, ];
	}

	if (!is.null(minimum.degree)){
		x.partitions <- x.partitions[x.partitions$..degree.. >= minimum.degree, ];
	}

	### sorting x for plotting
	if (!is.null(membership.order)){
		x.partitions <- x.partitions[match(membership.order, x.partitions$..set..),];
		}
	else if (1 == length(sorting)){
		if ('set' == sorting){
			x.partitions <- x.partitions
			}
		else if ('frequency' == sorting){
			x.partitions <- x.partitions[order(-x.partitions$..count..), ];
			if (sorting.reverse){
				x.partitions <- x.partitions[order(x.partitions$..count..), ];
				}
			}
		else if ('degree' == sorting) {
			x.partitions <- x.partitions[order(-x.partitions$..degree..), ];
			if (sorting.reverse){
				x.partitions <- x.partitions[order(x.partitions$..degree..), ];
				}
			}
		else {
			warning("The sorting parameter only accepts 'set', or 'frequency', 'degree' or the latter two in a vector, overwritten by membership.order.");
			}
	}
	else if (2 == length(sorting)){
		if (all(c('frequency', 'degree') == sorting)) {
			x.partitions <- x.partitions[order(-x.partitions$..count.., -x.partitions$..degree..), ];
			if (sorting.reverse){
				x.partitions <- x.partitions[order(x.partitions$..count.., x.partitions$..degree..), ];
				}
			}
		else if (all(c('degree', 'frequency') == sorting)) {
			x.partitions <- x.partitions[order(-x.partitions$..degree.., -x.partitions$..count..), ];
			if (sorting.reverse){
				x.partitions <- x.partitions[order(x.partitions$..degree.., x.partitions$..count..), ];
				}
			}
		else {
			warning("The sorting parameter only accepts 'frequency', 'degree' or both in a vector, overwritten by membership.order.");
			}
	}
	else {
		warning("The sorting parameter only accepts 'frequency', 'degree' or both in a vector, overwritten by membership.order.");
		}

	### list to store all three plots
	plots.list <- list()

	### data for intersection barplot (all y axis settings)
	intersection.barplot.data <- x.partitions;
	intersection.barplot.data$order <- 1:nrow(intersection.barplot.data);

	plots.list[['intersection.barplot']] <- create.barplot(
		formula = ..count.. ~ order,
		data = intersection.barplot.data,
		# groups = NULL,
		# stack = FALSE,
		# filename = NULL,
		# main = NULL,
		# main.just = 'center',
		# main.x = 0.5,
		# main.y = 0.5,
		# main.cex = 3,
		xlab.label = '',
		ylab.label = ylab.label,
		xlab.cex = 0,
		ylab.cex = ylab.cex,
		# xlab.col = 'black',
		ylab.col = ylab.col,
		xlab.top.label = xlab.top.label,
		xlab.top.cex = xlab.top.cex,
		xlab.top.col = xlab.top.col,
		xlab.top.just = xlab.top.just,
		xlab.top.x = xlab.top.x,
		xlab.top.y = xlab.top.y,
		abline.h = int.abline.h,
		abline.v = int.abline.v,
		abline.lty = int.abline.lty,
		abline.lwd = int.abline.lwd,
		abline.col = int.abline.col,
		axes.lwd = axes.lwd,
		add.grid = int.add.grid,
		xgrid.at = int.xgrid.at,
		ygrid.at = int.ygrid.at,
		grid.lwd = int.grid.lwd,
		grid.col = int.grid.col,
		xaxis.lab = FALSE,
		yaxis.lab = yaxis.lab,
		# xaxis.col = 'black',
		yaxis.col = yaxis.col,
		# xaxis.fontface = 'bold',
		yaxis.fontface = yaxis.fontface,
		xaxis.cex = 0,
		yaxis.cex = yaxis.cex,
		# xaxis.rot = 0,
		yaxis.rot = yaxis.rot,
		xaxis.tck = 0,
		yaxis.tck = yaxis.tck,
		xlimits = c(0.5, nrow(x.partitions) + 0.5),
		ylimits = ylimits,
		xat = 1:nrow(x.partitions),
		yat = yat,
		# layout = NULL,
		# as.table = FALSE,
		# x.spacing = 0,
		# y.spacing = 0,
		# x.relation = 'same',
		# y.relation = 'same',
		# top.padding = 0.5,
		# bottom.padding = 1,
		# right.padding = 1,
		# left.padding = 1,
		# key.bottom = 0.1,
		ylab.axis.padding = ylab.axis.padding,
		xlab.axis.padding = 0,
		col = int.col,
		border.col = int.border.col,
		border.lwd = int.border.lwd,
		plot.horizontal = FALSE,
		background.col = int.background.col,
		# origin = 0,
		reference = int.reference,
		box.ratio = int.box.ratio,
		# sample.order = 'none',
		# group.labels = FALSE,
		# key = list(text = list(lab = c(''))),
		# legend = NULL,
		add.text = int.add.text,
		text.labels = int.text.labels,
		text.x = int.text.x,
		text.y = int.text.y,
		text.col = int.text.col,
		text.cex = int.text.cex,
		text.fontface = int.text.fontface,
		strip.col = strip.col,
		strip.cex = strip.cex,
		# y.error.up = NULL,
		# y.error.down = y.error.up,
		# y.error.bar.col = 'black',
		# error.whisker.width = width/(nrow(data)*4),
		# error.bar.lwd = 1,
		# error.whisker.angle = 90,
		add.rectangle = int.add.rectangle,
		xleft.rectangle = int.xleft.rectangle,
		ybottom.rectangle = int.ybottom.rectangle,
		xright.rectangle = int.xright.rectangle,
		ytop.rectangle = int.ytop.rectangle,
		col.rectangle = int.col.rectangle,
		alpha.rectangle = int.alpha.rectangle,
		# line.func = NULL,
		# line.from = 0,
		# line.to = 0,
		# line.col = 'transparent',
		# line.infront = TRUE,
		text.above.bars = int.text.above.bars,
		raster = raster,
		raster.vert = raster.vert,
		raster.just = raster.just,
		raster.width.dim = raster.width.dim,
		# height = 6,
		# width = 6,
		size.units = size.units,
		resolution = resolution,
		enable.warnings = enable.warnings,
		description = description,
		style = style,
		preload.default = preload.default,
		use.legacy.settings = use.legacy.settings,
		# inside.legend.auto = FALSE,
		# disable.factor.sorting = FALSE
		);

	### data for membership scatterplot
	membership.scatterplot.matrix <- t(x.partitions[, set.order])
	colnames(membership.scatterplot.matrix) <- NULL
	rownames(membership.scatterplot.matrix) <- NULL

	membership.scatterplot.data <- data.frame(x = rep(x = 1:ncol(membership.scatterplot.matrix), times = nrow(membership.scatterplot.matrix)),
										y = rep(x = 1:nrow(membership.scatterplot.matrix), each = ncol(membership.scatterplot.matrix)))
	membership.scatterplot.data <- membership.scatterplot.data[order(membership.scatterplot.data$x, membership.scatterplot.data$y), ]

	membership.scatterplot.data$mem <- apply(membership.scatterplot.data, 1, function(x) membership.scatterplot.matrix[x['y'], x['x']])
	membership.scatterplot.data$col <- ifelse(membership.scatterplot.matrix[membership.scatterplot.data$y, membership.scatterplot.data$x], dot.col, dot.fill.col)
	membership.scatterplot.data$col <- dot.fill.col
	membership.scatterplot.data$col.border <- dot.fill.col.border
	membership.scatterplot.data$group <- membership.scatterplot.data$x

	if (1 == length(dot.col)){
		membership.scatterplot.data$col[membership.scatterplot.data$mem] <- dot.col
		}
	else {
		# TODO
		# membership.scatterplot.data$col[membership.scatterplot.data$mem] <- dot.col[membership.scatterplot.data$group[membership.scatterplot.data$mem]]
		}

	if (1 == length(dot.col.border)){
		membership.scatterplot.data$col.border[membership.scatterplot.data$mem] <- dot.col.border
		}
	else {
		# TODO
		# membership.scatterplot.data$col.border[membership.scatterplot.data$mem] <- dot.col.border[membership.scatterplot.data$group[membership.scatterplot.data$mem]]
		}

	membership.scatterplot.data$y <- nrow(membership.scatterplot.matrix) - membership.scatterplot.data$y + 1

	membership.scatterplot.data$start <- 0
	membership.scatterplot.data$end <- 0
	membership.scatterplot.data$line.col <- 'transparent'
	membership.scatterplot.data$line.lwd <- 0

	membership.scatterplot.data.split <- split(membership.scatterplot.data, membership.scatterplot.data$group);
	membership.scatterplot.data.split <- lapply(membership.scatterplot.data.split, function(x){
		x[1, ]$start <- max(x$y[x$mem]);
		x[1, ]$end <- min(x$y[x$mem]);
		x[1, ]$line.col <- ifelse(1 == length(line.col), line.col, line.col[x$group[1]]);
		x[1, ]$line.lwd <- ifelse(1 == length(line.lwd), line.lwd, line.lwd[x$group[1]]);
		return(x);
		})
	membership.scatterplot.data <- data.frame(do.call(rbind, membership.scatterplot.data.split))

	if (!dot.add.rectangle){
		if (dot.background.rectangles == 'row'){
			dot.add.rectangle <- TRUE;
			background.rectangles = data.frame(
				xleft.rectangle = rep(0.5, nrow(membership.scatterplot.matrix)),
				xright.rectangle = rep(ncol(membership.scatterplot.matrix) + 0.5, nrow(membership.scatterplot.matrix)),
				ybottom.rectangle = seq(0.5, nrow(membership.scatterplot.matrix) - 0.5, 1),
				ytop.rectangle = seq(1.5, nrow(membership.scatterplot.matrix) + 0.5, 1)
				);
			background.rectangles$col.rectangle <- 'transparent';
			background.rectangles$col.rectangle[seq(1, nrow(background.rectangles), 2)] <- dot.col.rectangle;
			background.rectangles$alpha.rectangle <- dot.alpha.rectangle;
			}
		else if (dot.background.rectangles == 'column'){
			dot.add.rectangle <- TRUE;
			background.rectangles = data.frame(
				xleft.rectangle = seq(0.5, ncol(membership.scatterplot.matrix) - 0.5, 1),
				xright.rectangle = seq(1.5, ncol(membership.scatterplot.matrix) + 0.5, 1),
				ybottom.rectangle = rep(0.5, ncol(membership.scatterplot.matrix)),
				ytop.rectangle = rep(nrow(membership.scatterplot.matrix) + 0.5, ncol(membership.scatterplot.matrix))
				);
			background.rectangles$col.rectangle <- 'transparent';
			background.rectangles$col.rectangle[seq(1, nrow(background.rectangles), 2)] <- dot.col.rectangle;
			background.rectangles$alpha.rectangle <- dot.alpha.rectangle;
			}
		else if (dot.background.rectangles == 'none'){
			dot.add.rectangle <- FALSE;
			}
		else {
			dot.add.rectangle <- FALSE;
			warning('The dot.background.rectangles option only supports row and column options, overwritten by dot.add.rectangle.');
			}
		}
	else {
		background.rectangles = data.frame(
			xleft.rectangle = dot.xleft.rectangle,
			ybottom.rectangle = dot.ybottom.rectangle,
			xright.rectangle = dot.xright.rectangle,
			ytop.rectangle = dot.ytop.rectangle,
			col.rectangle = dot.col.rectangle,
			alpha.rectangle = dot.alpha.rectangle,
			)
		}

	plots.list[['membership.scatterplot']] <- create.scatterplot(
		formula = y ~ x,
		data = membership.scatterplot.data,
		# filename = NULL,
		# groups = NULL,
		# main = NULL,
		# main.just = 'center',
		# main.x = 0.5,
		# main.y = 0.5,
		# main.cex = 3,
		xlab.label = '',
		ylab.label = '',
		xlab.cex = 0,
		ylab.cex = 0,
		# xlab.col = 'black',
		# ylab.col = 'black',
		# xlab.top.label = NULL,
		# xlab.top.cex = 2,
		# xlab.top.col = 'black',
		# xlab.top.just = 'center',
		# xlab.top.x = 0.5,
		# xlab.top.y = 0,
		xlimits = c(0.5, ncol(membership.scatterplot.matrix) + 0.5),
		ylimits = c(0.5, nrow(membership.scatterplot.matrix) + 0.5),
		xat = 1:length(unique(membership.scatterplot.data$group)),
		yat = 1:nrow(membership.scatterplot.matrix),
		xaxis.lab = NA,
		yaxis.lab = rev(set.order),
		xaxis.log = FALSE,
		yaxis.log = FALSE,
		xaxis.cex = 0,
		yaxis.cex = dot.yaxis.cex,
		xaxis.rot = 0,
		yaxis.rot = dot.yaxis.rot,
		xaxis.fontface = 'bold',
		yaxis.fontface = dot.yaxis.fontface,
		xaxis.col = 'black',
		yaxis.col = dot.yaxis.col,
		xaxis.tck = c(0,0),
		yaxis.tck = dot.yaxis.tck,
		add.grid = dot.add.grid,
		xgrid.at = 1:length(unique(membership.scatterplot.data$group)),
		ygrid.at = 1:nrow(membership.scatterplot.matrix),
		grid.colour = dot.grid.colour,
		horizontal = FALSE,
		type = 'p',
		cex = cex,
		pch = pch,
		col = membership.scatterplot.data$col,
		col.border = membership.scatterplot.data$col.border,
		# lwd = 1,
		# lty = 1,
		alpha = alpha,
		axes.lwd = axes.lwd,
		strip.col = strip.col,
		strip.cex = strip.cex,
		strip.fontface = strip.fontface,
		# y.error.up = NULL,
		# y.error.down = y.error.up,
		# x.error.right = NULL,
		# x.error.left = x.error.right,
		# y.error.bar.col = 'black',
		# x.error.bar.col = y.error.bar.col,
		# error.whisker.angle = 90,
		# error.bar.lwd = 1,
		# error.bar.length = 0.1,
		# key = list(text = list(lab = c(''))),
		# legend = NULL,
		# top.padding = 0.1,
		# bottom.padding = 0.7,
		# right.padding = 0.1,
		# left.padding = 0.5,
		# key.top = 0.1,
		# key.left.padding = 0,
		ylab.axis.padding = ylab.axis.padding,
		# axis.key.padding = 1,
		# layout = NULL,
		# as.table = FALSE,
		# x.spacing = 0,
		# y.spacing = 0,
		# x.relation = 'same',
		# y.relation = 'same',
		add.axes = add.axes,
		axes.lty = axes.lty,
		# add.xyline = FALSE,
		# xyline.col = 'black',
		# xyline.lwd = 1,
		# xyline.lty = 1,
		abline.h = dot.abline.h,
		abline.v = dot.abline.v,
		abline.col = dot.abline.col,
		abline.lwd = dot.abline.lwd,
		abline.lty = dot.abline.lty,
		# add.curves = FALSE,
		# curves.exprs = NULL,
		# curves.from = min(data, na.rm = TRUE),
		# curves.to = max(data, na.rm = TRUE),
		# curves.col = 'black',
		# curves.lwd = 2,
		# curves.lty = 1,
		add.rectangle = dot.add.rectangle,
		xleft.rectangle = background.rectangles$xleft.rectangle,
		ybottom.rectangle = background.rectangles$ybottom.rectangle,
		xright.rectangle = background.rectangles$xright.rectangle,
		ytop.rectangle = background.rectangles$ytop.rectangle,
		col.rectangle = background.rectangles$col.rectangle,
		alpha.rectangle = background.rectangles$alpha.rectangle,
		# add.points = FALSE,
		# points.x = NULL,
		# points.y = NULL,
		# points.pch = 19,
		# points.col = 'black',
		# points.col.border = 'black',
		# points.cex = 1,
		add.line.segments = TRUE,
		line.start = list(membership.scatterplot.data$start),
		line.end = list(membership.scatterplot.data$end),
		line.col = list(membership.scatterplot.data$line.col),
		line.lwd = list(membership.scatterplot.data$line.lwd),
		# add.text = FALSE,
		# text.labels = NULL,
		# text.x = NULL,
		# text.y = NULL,
		# text.col = 'black',
		# text.cex = 1,
		# text.fontface = 'bold',
		# text.guess.labels = FALSE,
		# text.guess.skip.labels = TRUE,
		# text.guess.ignore.radius = FALSE,
		# text.guess.ignore.rectangle = FALSE,
		# text.guess.radius.factor = 1,
		# text.guess.buffer.factor = 1,
		# text.guess.label.position = NULL,
		# height = 6,
		# width = 6,
		size.units = size.units,
		resolution = resolution,
		enable.warnings = enable.warnings,
		description = description,
		style = style,
		preload.default = preload.default,
		group.specific.colouring = FALSE,
		use.legacy.settings = use.legacy.settings,
		# inside.legend.auto = FALSE,
		# regions.labels = c(),
		# regions.start = c(),
		# regions.stop = c(),
		# regions.color = c("red"),
		# regions.cex = 1,
		# regions.alpha = 1,
		# lollipop.bar.y = NULL,
		# lollipop.bar.color = "gray",
		# ...
		);

	### data for set barplot (all x axis settings)
	x <- x[set.order]
	set.barplot.data <- data.frame(value = sapply(x, length), order = length(set.order):1);

	plots.list[['set.barplot']] <- create.barplot(
		formula = order ~ value,
		data = set.barplot.data,
		# groups = NULL,
		# stack = FALSE,
		# filename = NULL,
		# main = NULL,
		# main.just = 'center',
		# main.x = 0.5,
		# main.y = 0.5,
		# main.cex = 3,
		xlab.label = xlab.label,
		ylab.label = '',
		xlab.cex = xlab.cex,
		ylab.cex = 0,
		xlab.col = xlab.col,
		# ylab.col = 'black',
		# xlab.top.label = NULL,
		# xlab.top.cex = 2,
		# xlab.top.col = 'black',
		# xlab.top.just = 'center',
		# xlab.top.x = 0.5,
		# xlab.top.y = 0,
		abline.h = set.abline.h,
		abline.v = set.abline.v,
		abline.lty = set.abline.lty,
		abline.lwd = set.abline.lwd,
		abline.col = set.abline.col,
		axes.lwd = axes.lwd,
		add.grid = set.add.grid,
		xgrid.at = set.xgrid.at,
		ygrid.at = set.ygrid.at,
		grid.lwd = set.grid.lwd,
		grid.col = set.grid.col,
		xaxis.lab = xaxis.lab,
		yaxis.lab = FALSE,
		xaxis.col = xaxis.col,
		# yaxis.col = 'black',
		xaxis.fontface = xaxis.fontface,
		# yaxis.fontface = 'bold',
		xaxis.cex = xaxis.cex,
		yaxis.cex = 0,
		xaxis.rot = xaxis.rot,
		# yaxis.rot = 0,
		xaxis.tck = xaxis.tck,
		yaxis.tck = 0,
		xlimits = xlimits,
		ylimits = c(0.5, length(set.order) + 0.5),
		xat = xat,
		yat = 1:length(set.order),
		# layout = NULL,
		# as.table = FALSE,
		# x.spacing = 0,
		# y.spacing = 0,
		# x.relation = 'same',
		# y.relation = 'same',
		# top.padding = 0.5,
		# bottom.padding = 1,
		# right.padding = 1,
		# left.padding = 1,
		# key.bottom = 0.1,
		ylab.axis.padding = 0,
		xlab.axis.padding = xlab.axis.padding,
		col = set.col,
		border.col = set.border.col,
		border.lwd = set.border.lwd,
		plot.horizontal = TRUE,
		background.col = set.background.col,
		# origin = 0,
		reference = set.reference,
		box.ratio = set.box.ratio,
		# sample.order = 'none',
		# group.labels = FALSE,
		# key = list(text = list(lab = c(''))),
		# legend = NULL,
		add.text = set.add.text,
		text.labels = set.text.labels,
		text.x = set.text.x,
		text.y = set.text.y,
		text.col = set.text.col,
		text.cex = set.text.cex,
		text.fontface = set.text.fontface,
		strip.col = strip.col,
		strip.cex = strip.cex,
		# y.error.up = NULL,
		# y.error.down = y.error.up,
		# y.error.bar.col = 'black',
		# error.whisker.width = width/(nrow(data)*4),
		# error.bar.lwd = 1,
		# error.whisker.angle = 90,
		add.rectangle = set.add.rectangle,
		xleft.rectangle = set.xleft.rectangle,
		ybottom.rectangle = set.ybottom.rectangle,
		xright.rectangle = set.xright.rectangle,
		ytop.rectangle = set.ytop.rectangle,
		col.rectangle = set.col.rectangle,
		alpha.rectangle = set.alpha.rectangle,
		# line.func = NULL,
		# line.from = 0,
		# line.to = 0,
		# line.col = 'transparent',
		# line.infront = TRUE,
		text.above.bars = set.text.above.bars,
		raster = raster,
		raster.vert = raster.vert,
		raster.just = raster.just,
		raster.width.dim = raster.width.dim,
		# height = 6,
		# width = 6,
		size.units = size.units,
		resolution = resolution,
		enable.warnings = enable.warnings,
		description = description,
		style = style,
		preload.default = preload.default,
		use.legacy.settings = use.legacy.settings,
		# inside.legend.auto = FALSE,
		# disable.factor.sorting = FALSE
		);

	if (!include.set.barplot){
		plots.list[['set.barplot']] <- NULL;
		}

    # return separate plots
    if (return.list){
    	return(plots.list);
    	}

	# return multipanel plot or write plot based on filename
	if (include.set.barplot){

		upset.multipanelplot <- create.multipanelplot(
			plot.objects = plots.list,
			filename = filename,
			height = height,
			width = width,
			resolution = resolution,
			plot.objects.heights = plot.objects.heights,
			plot.objects.widths = plot.objects.widths,
			layout.width = 2,
			layout.height = 2,
			main = main,
			main.x = main.x,
			main.y = main.y,
			x.spacing = x.spacing,
			y.spacing = y.spacing,
			xlab.label = '',
			xlab.cex = 0,
			ylab.label = '',
			ylab.label.right = '',
			ylab.cex = 0,
			main.cex = main.cex,
			legend = legend,
			left.padding = left.padding,
			ylab.axis.padding = c(ylab.axis.padding, ylab.axis.padding),
			xlab.axis.padding = c(0, xlab.axis.padding),
			bottom.padding = bottom.padding,
			top.padding = top.padding,
			right.padding = right.padding,
			layout.skip = c(FALSE, TRUE, FALSE, FALSE),
			left.legend.padding = left.legend.padding,
			right.legend.padding = right.legend.padding,
			bottom.legend.padding = bottom.legend.padding,
			top.legend.padding = top.legend.padding,
			description = description,
			size.units = size.units,
			enable.warnings = enable.warnings,
			style = style,
			use.legacy.settings = use.legacy.settings
			);
		}
	else {
		plot.objects.widths <- plot.objects.widths[1]
		upset.multipanelplot <- create.multipanelplot(
			plot.objects = plots.list,
			filename = filename,
			height = height,
			width = width,
			resolution = resolution,
			plot.objects.heights = plot.objects.heights,
			plot.objects.widths = plot.objects.widths,
			layout.width = 1,
			layout.height = 2,
			main = main,
			main.x = main.x,
			main.y = main.y,
			x.spacing = x.spacing,
			y.spacing = y.spacing,
			xlab.label = '',
			xlab.cex = 0,
			ylab.label = '',
			ylab.label.right = '',
			ylab.cex = 0,
			main.cex = main.cex,
			legend = legend,
			left.padding = left.padding,
			ylab.axis.padding = c(ylab.axis.padding, ylab.axis.padding),
			xlab.axis.padding = c(0),
			bottom.padding = bottom.padding,
			top.padding = top.padding,
			right.padding = right.padding,
			layout.skip = c(FALSE, FALSE),
			left.legend.padding = left.legend.padding,
			right.legend.padding = right.legend.padding,
			bottom.legend.padding = bottom.legend.padding,
			top.legend.padding = top.legend.padding,
			description = description,
			size.units = size.units,
			enable.warnings = enable.warnings,
			style = style,
			use.legacy.settings = use.legacy.settings
			);
		}

	if (is.null(filename)){
		return(upset.multipanelplot)
	}

	}
