### Custom printing function for multipanel object
### This is needed because without it, the returned object is a grob which you must call grid.draw(x) on, this way, we can simply print the object.

print.multipanel <- function(x, ...) {
	## set class to that of a grid object
	class(x) <- c('frame', 'gTree', 'grob', 'gDesc');
	## close previous items if exist
	if (!is.null(dev.list()) && length(unlist(grid.ls(print = FALSE))) > 0) {
		grid.remove(grid.ls(print = FALSE)$name[1], redraw = TRUE);
		}
	## draw the plot like a grob
  	grid.draw(x);
	}

plot.multipanel <- function(x, ...) {
	## set class to that of a grid object
	class(x) <- c('frame', 'gTree', 'grob', 'gDesc');
	## close previous items if exist
	if (!is.null(dev.list()) && length(unlist(grid.ls(print = FALSE))) > 0) {
		grid.remove(grid.ls(print = FALSE)$name[1], redraw = TRUE);
		}
	## draw the plot like a grob
  	grid.draw(x);
	}

create.multipanelplot <- function(plot.objects = NULL, filename = NULL, height = 10, width = 10, resolution = 1000,
	plot.objects.heights = c(rep(1, layout.height)), plot.objects.widths = c(rep(1, layout.width)), layout.width = 1,
        layout.height = length(plot.objects), main = '', main.x = 0.5, main.y = 0.5, x.spacing = 0, y.spacing= 0,
        xlab.label = '', xlab.cex = 2, ylab.label = '', ylab.label.right = '',
        ylab.cex = 2, main.cex = 3, legend = NULL, left.padding = 0,
        ylab.axis.padding = c(rep(0, layout.width)), xlab.axis.padding = c(rep(0, layout.height)), bottom.padding = 0,
	top.padding = 0, right.padding = 0, layout.skip = c(rep(FALSE, layout.width * layout.height)), left.legend.padding = 2,
	right.legend.padding = 2, bottom.legend.padding = 2, top.legend.padding = 2,
        description = 'Created with BoutrosLab.plotting.general', size.units = 'in', enable.warnings = FALSE, style= 'BoutrosLab',
	use.legacy.settings = FALSE) {
        ## make axis.padding appropriate length if only 1 value specified
        if (1 == length(ylab.axis.padding)) {
                ylab.axis.padding <- rep(ylab.axis.padding, layout.width);
                }
        if (1 == length(xlab.axis.padding)) {
                xlab.axis.padding <- rep(xlab.axis.padding, layout.height);
                }

	### ERROR CHECKING ###
	if (length(plot.objects.heights) != layout.height) {
		stop('plot.objects.heights must have layout.height  number of entries');
		}
	if (length(plot.objects.widths) != layout.width) {
		stop('plot.objects.widths must have layout.width  number of entries');
		}
	if (length(layout.skip) != layout.width * layout.height) {
		stop('layout.skip must have same number of entries as layout.width * layout.height');
		}
	if (!is.list(plot.objects)) {
		stop('plot.objects must be a list');
		}
	if (length(ylab.axis.padding) != layout.width) {
		stop('ylab.axis.padding must be the same size as layout.width');
		}
	if (length(xlab.axis.padding) != layout.height) {
		stop('xlab.axis.padding must be the same size as layout.height');
		}



	padding.text.to.padding.ratio <- 6.04; # this is used to align plots with diffrent label sizes
  	tick.to.padding.ratio <- 0.9484252; # this is used to evaluate length of ticks (is equivalent to 1mm)
	additional.padding <- 1; # this is additional padding for labels that goes above calculation (takes care of overhanging values)

	### MINOR ADJUSTMENTS TO MAKE VALUES SEEM MORE LOGICAL
	x.spacing <- x.spacing / 10;
	y.spacing <- y.spacing / 10;

	### HANDLE SKIP LAYOUT ###
	newplots <- list();
	plotnum <- 1;

	## ADD BLANKS FOR LAYOUT.SKIP
	for (i in c(1:(layout.width * layout.height))) {
		if (plotnum <= length(plot.objects) && FALSE == layout.skip[i]) {
			newplots[i] <- plot.objects[plotnum];
			plotnum <- plotnum + 1;
			}
		else {
			newplots[[i]] <- rectGrob(gp = gpar(col = 'white', alpha = 0));
			}
		}

	plot.objects <- newplots;
	layout.height <- layout.height * 2;
	layout.width <- layout.width * 2;


	newplots <- list();
	plotnum <- 1;

	### ADD IN BETWEEN GROBS TO REPRESENT X AND Y SPACING (PREVENTS PUSHING OF EDGE PLOTS)
	for (k in c(1:layout.height)) {
		for (i in c(1:layout.width)) {
			if (0 == i %% 2 || 0 == k %% 2) {
				newplots[[i + (k - 1) * (layout.width)]] <- rectGrob(gp = gpar(col = 'white', alpha = 0));
				}
			else {
				newplots[i + (k - 1) * (layout.width)] <- plot.objects[plotnum];
				plotnum <- plotnum + 1;
				}
			}
		}

	plot.objects <- newplots;

	### PAR SETTINGS EQUIVALENT ###
	for (i in c(1:length(plot.objects))) {
      		# make all paddings the same (in order to line them up) -- needed or plots wont line up properly
		if (!is.null(plot.objects[[i]]$par.settings)) {

			plot.objects[[i]]$par.settings$layout.widths$left.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$key.left <- 1;
			plot.objects[[i]]$par.settings$layout.widths$key.ylab.padding <- 1;
			plot.objects[[i]]$par.settings$layout.widths$ylab <- 1;
			plot.objects[[i]]$par.settings$layout.widths$ylab.axis.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.left <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.right <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.key.padding <- 1;
			plot.objects[[i]]$par.settings$layout.widths$key.right <- 1;
			plot.objects[[i]]$par.settings$layout.widths$right.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$top.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$main <- 1;
 			plot.objects[[i]]$par.settings$layout.heights$main.key.padding <- 1;
			plot.objects[[i]]$par.settings$layout.heights$key.top <- 1;
			plot.objects[[i]]$par.settings$layout.heights$key.axis.padding <- 1;
			plot.objects[[i]]$par.settings$layout.heights$axis.top <- 0;
			plot.objects[[i]]$par.settings$layout.heights$axis.bottom <- 0;
			plot.objects[[i]]$par.settings$layout.heights$axis.xlab.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$xlab <- 1;
			plot.objects[[i]]$par.settings$layout.heights$xlab.key.padding <- 1;
			plot.objects[[i]]$par.settings$layout.heights$key.bottom <- 1;
			plot.objects[[i]]$par.settings$layout.heights$key.sub.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$sub <- 0;
			plot.objects[[i]]$par.settings$layout.heights$bottom.padding <- 0;
			if (use.legacy.settings) {
				plot.objects[[i]]$main$fontfamily <- 'Arial';
				plot.objects[[i]]$ylab$fontfamily <- 'Arial';
				plot.objects[[i]]$xlab$fontfamily <- 'Arial';
				plot.objects[[i]]$xlab.top$fontfamily <- 'Arial';
				plot.objects[[i]]$x.scales$fontfamily <- 'Arial';
				plot.objects[[i]]$y.scales$fontfamily <- 'Arial';
				}
			}
      		}



	### SPACING #####
	#make y spacing into an array of proper length
	if (1 == length(y.spacing)) {
		y.spacing <- rep(y.spacing, layout.height / 2);
		}

	if (1 == length(x.spacing)) {
		x.spacing <- rep(x.spacing, layout.width / 2);
		}

	# adjust widths to accuratly using the x and y spacing values
  	plot.objects.widths <- as.vector(rbind(plot.objects.widths, x.spacing));
	plot.objects.heights <- as.vector(rbind(plot.objects.heights, y.spacing));
	plot.objects.heights[length(plot.objects.heights)] <- 0;
	plot.objects.widths[length(plot.objects.widths)] <- 0;



	### PADDING ####

	#variables to represent amount of padding needed
	### X AND Y PLOT AXIS LABELS
	largest.y.axis <- list();
	largest.x.axis <- list();
	largest.top.x.axis <- list();
	### TICKS
	left.ticks <- list();
	bottom.ticks <- list();
	### X AND Y AXIS LABELS
	y.label.size <- list();
	x.label.size <- list();
	### MAIN PLOT LABEL
	main.size <- list();
	### LEGENDS
	left.legend <- 0;
	right.legend <- 0;
	bottom.legend <- 0;
	top.legend <- 0;

	for (i in  c(1:length(plot.objects))) {

		largest.y.axis[i] <- 0;
		left.ticks[i] <- 0;
		largest.x.axis[i] <- 0;
		bottom.ticks[i] <- 0;
		x.label.size[i] <- 0;
		y.label.size[i] <- 0;
		main.size[i] <- 0;
		left.legend[i] <- 0;
		right.legend[i] <- 0;
		top.legend[i] <- 0;
		bottom.legend[i] <- 0;
		largest.top.x.axis[i] <- 0;
    		# set variables for each plot according to their respective plot values
		if (!is.null(plot.objects[[i]]$par.settings)) {
			### GET GROB INFO FOR SIMPLE VARS
			top.legend[i] <- get.legend.height(plot.objects[[i]]$legend$top, filename, width, height, resolution);
			bottom.legend[i] <- get.legend.height(plot.objects[[i]]$legend$bottom, filename, width, height, resolution);
			left.legend[i] <- get.legend.width(plot.objects[[i]]$legend$left, filename, width, height, resolution);
			right.legend[i] <- get.legend.width(plot.objects[[i]]$legend$right, filename, width, height, resolution);
			main.size[i] <- get.text.grob.height(plot.objects[[i]]$main$label, plot.objects[[i]]$main$cex,
				0, filename, width, height, resolution);
			y.label.size[i] <- get.text.grob.width(plot.objects[[i]]$ylab$label, plot.objects[[i]]$ylab$cex,
				90, filename, width, height, resolution);
			x.label.size[i] <- get.text.grob.height(plot.objects[[i]]$xlab$label, plot.objects[[i]]$xlab$cex,
				0, filename, width, height, resolution);
			left.ticks[i] <- plot.objects[[i]]$y.scales$tck[1] * tick.to.padding.ratio;
			bottom.ticks[i] <- plot.objects[[i]]$x.scales$tck[1] * tick.to.padding.ratio;
			## from data and labels -- get the actual labels that will be in plot as strings
			y.axis.labs <- reformat.labels(plot.objects[[i]]$y.scales$labels, plot.objects[[i]]$panel.args[[1]]$y);
			x.axis.labs <- reformat.labels(plot.objects[[i]]$x.scales$labels, plot.objects[[i]]$panel.args[[1]]$x);
			x.axis.top.labs <- reformat.labels(plot.objects[[i]]$xlab.top$label, NULL);
			
			largest.y.axis[i] <- get.text.grob.width(
				y.axis.labs,
				plot.objects[[i]]$y.scales$cex,
				plot.objects[[i]]$y.scales$rot[1],
				filename,
				width,
				height,
				resolution
				);
			largest.x.axis[i] <- get.text.grob.height(
				x.axis.labs,
				plot.objects[[i]]$x.scales$cex,
				plot.objects[[i]]$x.scales$rot[1],
				filename,
				width,
				height,
				resolution
				);
			largest.top.x.axis[i] <- get.text.grob.height(
                                x.axis.top.labs,
				plot.objects[[i]]$xlab.top$cex,
                                0,
                                filename,
                                width,
                                height,
                                resolution
                                );
			}
		}

	# add the correct amount of padding to each plot based on the column (use ylab axis)
	for (i in c(1:layout.width)) {

		max.axis <- max(unlist(largest.y.axis[seq(i, length(plot.objects), layout.width)]));
		max.ticks <- max(unlist(left.ticks[seq(i, length(plot.objects), layout.width)]));
		max.labels <- max(unlist(y.label.size[seq(i, length(plot.objects), layout.width)]));
		max.right.legend <- max(unlist(right.legend[seq(i, length(plot.objects), layout.width)]));
		max.left.legend <- max(unlist(left.legend[seq(i, length(plot.objects), layout.width)]));

		for (j in seq(i, length(plot.objects), layout.width)) {
			diff.max.labels <- abs(max.labels - y.label.size[[j]]);
			to.add <- (max.axis + diff.max.labels + max.labels/2) / padding.text.to.padding.ratio + max.ticks + additional.padding;

			if (!is.null(plot.objects[[j]]$par.settings)) {

				diff.right.legend <- abs(right.legend[j] - max.right.legend) / padding.text.to.padding.ratio;
				diff.left.legend <- abs(left.legend[j] - max.left.legend) / padding.text.to.padding.ratio;
				
				#if(diff.right.legend != 0) {diff.right.legend <- diff.right.legend + 1 / padding.text.to.padding.ratio}
				if(diff.left.legend != 0) {diff.left.legend <- diff.left.legend + 1 / padding.text.to.padding.ratio}

				### if has legend, must account for minor key padding

				plot.objects[[j]]$par.settings$layout.widths$ylab.axis.padding <- ylab.axis.padding[ceiling(i / 2)] + to.add;
				plot.objects[[j]]$par.settings$layout.widths$left.padding <- plot.objects[[j]]$par.settings$layout.widths$left.padding + diff.left.legend;
				plot.objects[[j]]$par.settings$layout.widths$right.padding <- plot.objects[[j]]$par.settings$layout.widths$right.padding + diff.right.legend;

				}
			}
		}


	# add the correct amount of padding to each plot based on the row (use xlab axis)
	for (i in seq(1, length(plot.objects), layout.width)) {

		max.axis <- max(unlist(largest.x.axis[seq(i, i + layout.width - 1, 1)]));
		max.ticks <- max(unlist(bottom.ticks[seq(i, i + layout.width - 1, 1)]));
		max.labels <- max(unlist(x.label.size[seq(i, i + layout.width - 1, 1)]));
		max.main <- max(unlist(main.size[seq(i, i + layout.width - 1, 1)])) / 2;
		max.top.legend <- max(unlist(top.legend[seq(i, i + layout.width - 1, 1)]));
		max.bottom.legend <- max(unlist(bottom.legend[seq(i, i + layout.width - 1, 1)]));
		max.axis.top <- max(unlist(largest.top.x.axis[seq(i, i + layout.width - 1, 1)]));

		for (j in c(i:(i + layout.width - 1))) {
			diff.max.labels <- abs(max.labels - x.label.size[[j]]);
			to.add <- (max.axis + diff.max.labels + max.labels/2) / padding.text.to.padding.ratio + max.ticks + additional.padding;
			
			if (j <= length(plot.objects) && !is.null(plot.objects[[j]]$par.settings)) {

				diff.top.legend <- abs(top.legend[j] - max.top.legend) / padding.text.to.padding.ratio;
				diff.bottom.legend <- abs(bottom.legend[j] - max.bottom.legend) / padding.text.to.padding.ratio;
				diff.axis.top <- abs(largest.top.x.axis[[j]] - max.axis.top) / padding.text.to.padding.ratio;
				
				#if(diff.top.legend != 0) {diff.top.legend <- diff.top.legend + 1 / padding.text.to.padding.ratio}
                                if(diff.bottom.legend != 0) {diff.bottom.legend <- diff.bottom.legend + 1 / padding.text.to.padding.ratio}

				### if has legend, must account for minor key padding

				plot.objects[[j]]$par.settings$layout.heights$axis.xlab.padding <- xlab.axis.padding[ceiling(i / (layout.width * 2))] + to.add;

				if (max.main != 0 && (is.null(plot.objects[[j]]$main$label) || plot.objects[[j]]$main$label == '')) {
					plot.objects[[j]]$main$label <- '\t'; #make sure it thinks a label is there
					}

				plot.objects[[j]]$par.settings$layout.heights$top.padding <- plot.objects[[j]]$par.settings$layout.heights$top.padding + diff.top.legend + diff.axis.top;
				plot.objects[[j]]$par.settings$layout.heights$bottom.padding <-
					plot.objects[[j]]$par.settings$layout.heights$bottom.padding + diff.bottom.legend;
				plot.objects[[j]]$par.settings$layout.heights$main.key.padding <-
					plot.objects[[j]]$par.settings$layout.heights$main.key.padding + (max.main / padding.text.to.padding.ratio);

				}
			}
		}



	#grobs representing main labels on each side
	main.label <- textGrob(main, gp = gpar(cex = main.cex, fontface = 'bold',
			fontfamily = get.defaults(property = 'fontfamily', use.legacy.settings = use.legacy.settings  || ('Nature' == style))),
		x = main.x,
		y = main.y);
	y.label <- textGrob(ylab.label, gp = gpar(cex = ylab.cex, fontface = 'bold',
			fontfamily = get.defaults(property = 'fontfamily', use.legacy.settings = use.legacy.settings || ('Nature' == style))),
		rot = 90);
	y.label.right <- textGrob(ylab.label.right, gp = gpar(cex = ylab.cex, fontface = 'bold',
			fontfamily = get.defaults(property = 'fontfamily', use.legacy.settings = use.legacy.settings || ('Nature' == style))),
		rot = -90);
	x.label <- textGrob(xlab.label, gp = gpar(cex = xlab.cex, fontface = 'bold',
			fontfamily = get.defaults( property = 'fontfamily', use.legacy.settings = use.legacy.settings || ('Nature' == style)))
		);

	### IF LEGENDS ARE KEYS, MUST BE MADE INTO GROBS FIRST
  	if (identical(legend$left$fun, draw.key) || identical(legend$left$fun, draw.colorkey)) {
		legend$left$fun <- do.call(legend$left$fun, list(key = legend$left$args$key));
		}
  	if (identical(legend$right$fun, draw.key) || identical(legend$right$fun, draw.colorkey)) {
		legend$right$fun <- do.call(legend$right$fun, list(key = legend$right$args$key));
		}
  	if (identical(legend$bottom$fun, draw.key) || identical(legend$bottom$fun, draw.colorkey)) {
		legend$bottom$fun <- do.call(legend$bottom$fun, list(key = legend$bottom$args$key));
		}
  	if (identical(legend$top$fun, draw.key) || identical(legend$top$fun, draw.colorkey)) {
		legend$top$fun <- do.call(legend$top$fun, list(key = legend$top$args$key));
		}
  	if (identical(legend$inside$fun, draw.key) || identical(legend$inside$fun, draw.colorkey)) {
		legend$inside$fun <- do.call(legend$inside$fun, list(key = legend$inside$args$key));
		}


	### LEFT,RIGHT,TOP,BOTTOM GROBS ############
	### LEFT GROB
	if (!is.null(legend$left$fun)) {

		width.legend <- convertUnit(
			grobWidth(legend$left$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);

    		width.text <- 0;

		if (ylab.label != '') {
			width.text <- convertUnit(
				grobWidth(y.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		left.layout.final <- grid.layout(
			nrow = 1,
			ncol = 4,
			widths = unit(
				x = c(left.padding, width.text, width.legend, left.legend.padding),
				units = c('lines', 'lines', 'lines', 'lines')
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);
		left.grob <- frameGrob(layout = left.layout.final);
		left.grob <- placeGrob(
			frame = left.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);

		left.grob <- placeGrob(
			frame = left.grob,
			grob = y.label,
			row = 1,
			col = 2
			);

		left.grob <- placeGrob(
			frame = left.grob,
			grob = legend$left$fun,
			row = 1,
			col = 3
			);
		left.grob <- placeGrob(
			frame = left.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 4
			);

		}
	else {

    		width.text <- 0;
		if (ylab.label != '') {
			width.text <- convertUnit(
				grobWidth(y.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		left.layout.final <- grid.layout(
			nrow = 1,
			ncol = 3,
			widths = unit(
				x = c(left.padding, width.text, left.legend.padding),
				units = c('lines', 'lines', 'lines')
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);
		left.grob <- frameGrob(layout = left.layout.final);
		left.grob <- placeGrob(
			frame = left.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);

		left.grob <- placeGrob(
			frame = left.grob,
			grob = y.label,
			row = 1,
			col = 2
			);
		left.grob <- placeGrob(
			frame = left.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 3
			);
		}

	### RIGHT GROB
	if (!is.null(legend$right$fun)) {
		width.legend <- convertUnit(
			grobWidth(legend$right$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);

    		width.text <- 0;
		if (ylab.label.right != '') {
			width.text <- convertUnit(
				grobWidth(y.label.right),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		right.layout.final <- grid.layout(
			nrow = 1,
			ncol = 4,
			widths = unit(
				x = c(right.legend.padding, width.legend, width.text, right.padding),
				units = c('lines', 'lines', 'lines', 'lines')
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);

		right.grob <- frameGrob(layout = right.layout.final);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = legend$right$fun,
			row = 1,
			col = 2
			);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = y.label.right,
			row = 1,
			col = 3
			);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 4
			);
		}
	else {

    		width.text <- 0;
		if (ylab.label.right != '') {
			width.text <- convertUnit(
				grobWidth(y.label.right),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		right.layout.final <- grid.layout(
			nrow = 1,
			ncol = 3,
			widths = unit(
				x = c(right.legend.padding, width.text, right.padding),
				units = c('lines', 'lines', 'lines')
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);

		right.grob <- frameGrob(layout = right.layout.final);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = y.label.right,
			row = 1,
			col = 2
			);
		right.grob <- placeGrob(
			frame = right.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 3
			);
		}

	### BOTTOM GROB
	if (!is.null(legend$bottom$fun)) {

		height.legend <- convertUnit(
			grobHeight(legend$bottom$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    		height.text <- 0;
		if (xlab.label != '') {
			height.text <- convertUnit(
				grobHeight(x.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		bottom.layout.final <- grid.layout(
			nrow = 4,
			ncol = 1,
			heights = unit(
				x = c(bottom.legend.padding, height.legend, height.text, bottom.padding),
				units = c('lines', 'lines', 'lines', 'lines')
				),
			widths = unit(1, 'null'),
			respect = FALSE
			);

		bottom.grob <- frameGrob(layout = bottom.layout.final);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = legend$bottom$fun,
			row = 2,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = x.label,
			row = 3,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 4,
			col = 1
			);
		}
	else {

    		height.text <- 0;
		if (xlab.label != '') {
			height.text <- convertUnit(
				grobHeight(x.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		bottom.layout.final <- grid.layout(
			nrow = 4,
			ncol = 1,
			heights = unit(
				x = c(bottom.legend.padding, height.text, bottom.padding),
				units = c('lines', 'lines', 'lines')
				),
			widths = unit(1, 'null'),
			respect = FALSE
			);

		bottom.grob <- frameGrob(layout = bottom.layout.final);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = x.label,
			row = 2,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 3,
			col = 1
			);
		}

	### TOP GROB
	if (!is.null(legend$top$fun)) {

		height.legend <- convertUnit(
			grobHeight(legend$top$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    		height.text <- 0;
		if (main != '') {
			height.text <- convertUnit(
				grobHeight(main.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}

		top.layout.final <- grid.layout(
			nrow = 4,
			ncol = 1,
			heights = unit(
				x = c(top.padding, height.text, height.legend, top.legend.padding),
				units = c('lines', 'lines', 'lines', 'lines')
      				),
			widths = unit(1, 'null'),
			respect = FALSE
			);

		top.grob <- frameGrob(layout = top.layout.final);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 1,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
			row = 3,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 2,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = legend$top$fun,
			row = 4,
			col = 1
			);
		}
	else {
		height.text <- 0;
		if (main != '') {
			height.text <- convertUnit(
				grobHeight(main.label),
				unitTo = 'lines',
				axisFrom = 'x',
				typeFrom = 'dimension',
				valueOnly = TRUE
				);
			}
		top.layout.final <- grid.layout(
			nrow = 3,
			ncol = 1,
			heights = unit(
				x = c(top.padding, height.text, top.legend.padding),
				units = c('lines', 'lines', 'lines')
				),
			widths = unit(1, 'null'),
			respect = FALSE
			);
		top.grob <- frameGrob(layout = top.layout.final);
		top.grob <- placeGrob(
			frame = top.grob,
      			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
      			row = 2,
      			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
      			grob = rectGrob(gp = gpar(col = 'white', alpha = 0)),
      			row = 1,
      			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 3,
			col = 1
			);
		}

	# create grob of all plots
	grob <- arrangeGrob(
		grobs = plot.objects,
		heights = plot.objects.heights,
    		widths = plot.objects.widths,
		ncol = layout.width,
		nrow = layout.height,
		top = top.grob,
		left = left.grob,
		bottom = bottom.grob,
		right = right.grob
		);
	## Add white background color
	grob <- gtable_add_grob(grob, grobs = rectGrob(gp = gpar(fill = 'white', lwd = 0)), 1, 1, nrow(grob), ncol(grob), 0);
	
	grob.layout <- grid.layout(1,1);
	grob.frame <- frameGrob(layout = grob.layout);
	grob.frame <- placeGrob(grob.frame,grob);
	#### INSIDE GROBS
	if (!is.null(legend$inside$fun)) {
		if (!is.null(legend$inside$x)) {
			legend$inside$fun$framevp$x <- unit(legend$inside$x, 'npc');
			}
		if (!is.null(legend$inside$y)) {
			legend$inside$fun$framevp$y <- unit(legend$inside$y, 'npc');
			}
		## add the inside legend to a frameGrob
		grob.frame <- placeGrob(grob.frame,legend$inside$fun);
		}
	grob <- grob.frame;
	


	# If Nature style requested, change figure accordingly
        if ('Nature' == style) {

                # Ensure sufficient resolution for graphs
                if (resolution < 1200) {
                        resolution <- 1200;
                        warning('Setting resolution to 1200 dpi.');
                        }

                # Other required changes which are not accomplished here
                warning('Nature also requires italicized single-letter variables and en-dashes for ranges and negatives.
			See example in documentation for how to do this.');

                warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend');
                }

	class(grob) <- 'multipanel';
	# return grob
	if (!is.null(filename)) {
		BoutrosLab.plotting.general::write.plot(
			trellis.object = grob,
			filename = filename,
			height = height,
			width = width,
			size.units = size.units,
			resolution = resolution,
			enable.warnings = enable.warnings,
			description = description
			);
		}
	else {

		# return grob itself
		return(grob);
		}
	}


### Labels can get a bit weird because they come in many shapes and forms
### (i.e, expressions, NA, NULL....) --- this is intended to check for all those
## from data and labels -- get the actual labels that will be in plot as strings
reformat.labels <- function(labels, data.values) {

	if (is.expression(labels[1])) {
        	y.axis.labs <- labels;
                }
        else if (is.null(labels) || anyNA(labels) || (length(labels) == 1 && labels == TRUE)) {
                if (!is.null(data.values)) {
                	if (!is.null(levels(data.values))) {
                        	y.axis.labs <- levels(data.values);
                                }
                        else {
                        	yvals <- as.numeric(data.values);
                        	y.axis.labs <-  pretty(yvals[is.finite(yvals)]);
                                }
                        }
                    else {
                            y.axis.labs <- '';
                            }
                    }
	else {
        	y.axis.labs <- labels;
        	}

	return(y.axis.labs);


}

get.legend.height <- function(legend, filename, width, height, resolution) {
	if (is.element('function', class(legend$fun))) {
	#if (class(legend$fun) == 'function') {
		legend$fun <- do.call(legend$fun, legend$args);
		}
	#grob size depends on image type and size -- must simulate opening the device
	extension <- file_ext(filename);
	if (!is.null(filename)) {
                if ('tiff' == extension) {
                        tiff(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('png' == extension) {
                        png(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('pdf' == extension) {
                        cairo_pdf(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('svg' == extension) {
                        svg(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('eps' == extension) {
                        postscript(height = height, width = width);
                        }
                else {
                        stop('File type not supported');
                        }
                }
	if (is.null(legend)) {
		height.legend <- 0;
		}
	else if (is.grob(legend$fun)) {
		height.legend <- convertUnit(
			grobHeight(legend$fun),
			unitTo = 'points',
			axisFrom = 'y',
			typeFrom = 'dimension'
			);
		}
	else {
		grob <- do.call(legend$fun, list(key = legend$args$key, draw = FALSE));
		height.legend <- convertUnit(
			grobHeight(grob),
			unitTo = 'points',
			axisFrom = 'y',
			typeFrom = 'dimension'
			);
		}

	if (!is.null(filename)) {
		dev.off()
		}
	return(as.integer(height.legend));

	}

get.legend.width <- function(legend, filename, width, height, resolution) {
	if (is.element('function', class(legend$fun))) {
		legend$fun <- do.call(legend$fun, legend$args);
		}
	#grob size depends on image type and size -- must simulate opening the device
	extension <- file_ext(filename);
	if (!is.null(filename)) {
                if ('tiff' == extension) {
                        tiff(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('png' == extension) {
                        png(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('pdf' == extension) {
                        cairo_pdf(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('svg' == extension) {
                        svg(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('eps' == extension) {
                        postscript(height = height, width = width);
                        }
                else {
                        stop('File type not supported');
                        }
                }
	if (is.null(legend)) {
		width.legend <- 0;
		}
	else if (is.grob(legend$fun)) {
		width.legend <- convertUnit(
			grobWidth(legend$fun),
			unitTo = 'points',
			axisFrom = 'x',
			typeFrom = 'dimension'
			);
		}
	else {
		grob <- do.call(legend$fun, list(key = legend$args$key, draw = FALSE));
		width.legend <- convertUnit(
			grobWidth(grob),
			unitTo = 'points',
			axisFrom = 'x',
			typeFrom = 'dimension'
			);
		}

	if (!is.null(filename)) {
		dev.off();
		}

	return(as.integer(width.legend));

	}

### function to get the grob width given the text and specification parameters##
get.text.grob.width <- function(labels, cex, rot, filename, width, height, resolution) {
	
	#grob size depends on image type and size -- must simulate opening the device
	if (is.null(labels)) {
		return(0);
		}
	extension <- file_ext(filename);
	if (!is.null(filename)) {
                if ('tiff' == extension) {
                        tiff(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('png' == extension) {
                        png(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
                        }
                else if ('pdf' == extension) {
                        cairo_pdf(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('svg' == extension) {
                        svg(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
                        }
                else if ('eps' == extension) {
                        postscript(height = height, width = width);
                        }
                else {
                        stop('File type not supported');
                        }
                }
        # if not an empty label or all blank, create the grob, and get its width
	if (0 < length(labels)  && !(all('' == as.character(labels)))) {
		grob <- textGrob(labels, gp = gpar(cex = cex, lineheight = 1), rot = rot, x = c(rep(0.5, length(labels))), y = c(rep(0.5, length(labels))));
		width.grob <- convertUnit(
			grobWidth(grob),
			unitTo = 'points',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
		}
	else {
		width.grob <- 0;
		}
	### make sure to turn off the dev or we will have one open for every time this is called
	if (!is.null(filename)) {
		dev.off();
		}

	return(width.grob);

	}

### function to get the grob height given the text and specification parameters##
get.text.grob.height <- function(labels, cex, rot, filename, width, height, resolution) {
	#grob size depends on image type and size -- must simulate opening the device
	if (is.null(labels)) {
		return(0);
		}
	extension <- file_ext(filename);
	if (!is.null(filename)) {
		if ('tiff' == extension) {
			tiff(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
			}
		else if ('png' == extension) {
			png(filename = paste0(tempdir(),'/temp-multipanel'), type = 'cairo', height = height, width = width, res = resolution);
			}
		else if ('pdf' == extension) {
			cairo_pdf(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
			}
		else if ('svg' == extension) {
			svg(filename = paste0(tempdir(),'/temp-multipanel'), height = height, width = width);
			}
		else if ('eps' == extension) {
			postscript(height = height, width = width);
			}
		else {
			stop('File type not supported');
			}
		}
	# if not an empty label or all blank, create the grob, and get its height
	if (0 < length(labels) && !(all('' == as.character(labels)))) {
		grob <- textGrob(labels, gp = gpar(cex = cex, lineheight = 1), rot = rot, x = c(rep(0.5, length(labels))), y = c(rep(0.5, length(labels))));
		height.grob <- convertUnit(
			grobHeight(grob),
			unitTo = 'points',
			axisFrom = 'y',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
		}
	else {
		height.grob <- 0;
		}
	### make sure to turn off the dev or we will have one open for every time this is called
	if (!is.null(filename)) {
		dev.off();
		}

	return(height.grob);

	}
