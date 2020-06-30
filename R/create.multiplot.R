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

### FUNCTION TO CREATE MULTIPLOT ###################################################################
create.multiplot <- function(plot.objects, filename = NULL, panel.heights = c(1, 1), panel.widths = 1, main = NULL,
	main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3, main.key.padding = 1,
	ylab.padding = 5, xlab.padding = 5, xlab.to.xaxis.padding = 2, right.padding = 1,
	left.padding = 1, top.padding = 0.5, bottom.padding = 0.5, xlab.label = NULL,
	ylab.label = NULL, xlab.cex = 2, ylab.cex = 2, xlab.top.label = NULL, xaxis.top.tck.lab = NULL, xat.top = TRUE, xlab.top.cex = 2,
	xaxis.top.idx = NULL, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0,
	xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.labels = TRUE, yaxis.labels = TRUE,
	xaxis.alternating = 1, yaxis.alternating = 1, xat = TRUE, yat = TRUE, xlimits = NULL,
	ylimits = NULL, xaxis.rot = 0, xaxis.rot.top = 0, xaxis.fontface = 'bold', y.tck.dist=0.5, x.tck.dist=0.5, yaxis.fontface = 'bold',
	x.spacing = 1, y.spacing = 1, x.relation = 'same', y.relation = 'same',
	xaxis.tck = c(0.75, 0.75), yaxis.tck = c(0.75, 0.75), axes.lwd = 1.5, key.right.padding = 1,
	key.left.padding = 1, key.bottom.padding = 1, xlab.key.padding = 0.5, height = 6, width = 6, size.units = 'in',
	resolution = 1600, enable.warnings = FALSE, key = list(text = list(lab = c(''))),
	legend =  NULL, print.new.legend = FALSE, merge.legends = FALSE,
	plot.layout = c(1, length(plot.objects)), layout.skip = rep(FALSE, length(plot.objects)),
	description = 'Created with BoutrosLab.plotting.general',
	plot.labels.to.retrieve = NULL, style = 'BoutrosLab', remove.all.border.lines = FALSE,
	preload.default = 'custom', plot.for.carry.over.when.same = 1, get.dendrogram.from = NULL,
	dendrogram.right.size = NULL, dendrogram.right.x = NULL, dendrogram.right.y = NULL,
	dendrogram.top.size = NULL, dendrogram.top.x = NULL, dendrogram.top.y = NULL, use.legacy.settings = FALSE) {

	if (preload.default == 'paper') {

		}
	else if (preload.default == 'web') {

		}
	# check that plots are trellis objects
	for (i in 1:length(plot.objects)) {
		if (class(plot.objects[[i]]) != 'trellis') {
			stop('Please only use trellis objects for this function');
			}
		}

	# Collect the axis limites, labels and at from all the plots
	xaxis.labels.plots <- list()[1:length(plot.objects)];
	yaxis.labels.plots <- list()[1:length(plot.objects)];
	xat.plots <- list()[1:length(plot.objects)];
	yat.plots <- list()[1:length(plot.objects)];
	xlimits.plots <- list()[1:length(plot.objects)];
	ylimits.plots <- list()[1:length(plot.objects)];

	# combine plot objects together and set layout
	combined.plot.objects <- c(plot.objects[[1]], layout = plot.layout);
	if (length(plot.objects) > 1) {
		for (i in 1:length(plot.objects)) {
			if (i > 1) {
				combined.plot.objects <- c(combined.plot.objects, plot.objects[[i]], layout = plot.layout, merge.legends = merge.legends);
				}
			# record each plot's axis values
			if (length(plot.objects[[i]]$x.scales$labels) > 0) {
				xaxis.labels.plots[[i]] <- plot.objects[[i]]$x.scales$labels;
				}
			if (length(plot.objects[[i]]$y.scales$labels) > 0) {
				yaxis.labels.plots[[i]] <- plot.objects[[i]]$y.scales$labels;
				}
		       	xat.plots[[i]] <- plot.objects[[i]]$x.scales$at;
		       	yat.plots[[i]] <- plot.objects[[i]]$y.scales$at;
			xlimits.plots[[i]] <- plot.objects[[i]]$x.limits;
		       	ylimits.plots[[i]] <- plot.objects[[i]]$y.limits;
			}
		}
	if (is.null(xaxis.top.idx)) {
		xaxis.top.idx <- length(plot.objects);
		}
	# specify tck marks for different alternating settings
	if (0 == xaxis.alternating) { xaxis.tck <- c(0, 0); }
	else if (1 == xaxis.alternating) { xaxis.tck[2] <- 0; }
	else if (2 == xaxis.alternating) { xaxis.tck[1] <- 0; }

	# specify tck marks for different alternating settings
	if (0 == yaxis.alternating) { yaxis.tck <- c(0, 0); }
	else if (1 == yaxis.alternating) { yaxis.tck[2] <- 0; }
	else if (2 == yaxis.alternating) { yaxis.tck[1] <- 0; }

	# if there are axis labels or tck locations for each plot, then the relations need to be free
	if ( (typeof(yaxis.labels) == 'list') || (typeof(yat) == 'list')) {
		y.relation <- 'free';
		}
	if ( (typeof(xaxis.labels) == 'list') || (typeof(xat) == 'list')) {
		x.relation <- 'free';
		}

	# if user asked to retrieve previous plot labels, then the relations need to be free
	if (!is.null(plot.labels.to.retrieve)) {
		y.relation <- 'free';
		x.relation <- 'free';
		}

	# Checks to see if there are NULL value(s) in the limit lists
	x.atleast.one.null <- FALSE;
	if (!is.null(xlimits) && is.null(plot.labels.to.retrieve)) {
		for (i in 1:length(xlimits)) {
			if (is.null(xlimits[[i]])) {
				x.atleast.one.null <- TRUE;
				break;
				}
			}
		}

	# If there are NULL value(s) or limit=NULL, then replace the NULL value(s)
	if (is.null(xlimits) || x.atleast.one.null) {
		xlimits <- replace.nulls(
			xlimits,
			xlimits.plots[[plot.for.carry.over.when.same]],
			xlimits.plots,
			x.relation);
		}

	if (!is.null(xat) && !anyNA(xat) && 1 == length(xat) && xat == TRUE) {
		xat <- if ('same' == x.relation) {xat.plots[[plot.for.carry.over.when.same]]} else {xat.plots};
		}
	if (!is.null(xaxis.labels) && !anyNA(xaxis.labels) && 1 == length(xaxis.labels) && xaxis.labels == TRUE) {
		xaxis.labels <- if ('same' == x.relation) {xaxis.labels.plots[[plot.for.carry.over.when.same]]} else {xaxis.labels.plots};
		}

	y.atleast.one.null <- FALSE;
	if (!is.null(ylimits) && is.null(plot.labels.to.retrieve)) {
		for (i in 1:length(ylimits)) {
			if (is.null(ylimits[[i]])) {
				y.atleast.one.null <- TRUE;
				break;
				}
			}
		}

	if (is.null(ylimits) || y.atleast.one.null) {
		ylimits <- replace.nulls(
			ylimits,
			ylimits.plots[[plot.for.carry.over.when.same]],
			ylimits.plots,
			y.relation);
		}

	if (!is.null(yat) && !anyNA(yat) && 1 == length(yat) && yat == TRUE) {
		yat <- if ('same' == y.relation) {yat.plots[[plot.for.carry.over.when.same]]} else {yat.plots};
		}
	if (!is.null(yaxis.labels) && !anyNA(yaxis.labels) && 1 == length(yaxis.labels) && yaxis.labels == TRUE) {
		yaxis.labels <- if ('same' == y.relation) {yaxis.labels.plots[[plot.for.carry.over.when.same]]} else {yaxis.labels.plots};
		}

	# consolidate all the parameters together for updating the lattice
	x.scale <- list(
		alternating = xaxis.alternating,
		tck = xaxis.tck,
		labels = xaxis.labels,
		cex = xaxis.cex,
		at = xat,
		rot = c(xaxis.rot, xaxis.rot.top),
		limits = xlimits,
		fontface = if ('Nature' == style) {'plain'} else (xaxis.fontface),
		relation = x.relation
		);
	y.scale <- list(
		alternating = yaxis.alternating,
		tck = yaxis.tck,
		labels = yaxis.labels,
		cex = yaxis.cex,
		at = yat,
		rot = 0,
		limits = ylimits,
		fontface = if ('Nature' == style) {'plain'} else (yaxis.fontface),
		relation = y.relation
		);

	# function to draw different top and bottom axes
	xscale.components.new <- function(...) {

		args <- xscale.components.default(...);
        	packet <- which.packet();
		if (!is.null(packet)) {
			if (packet == xaxis.top.idx) {
				args$top <- args$bottom;
        			if (length(xat.top) == 0) {
            				xat.top <- c(1:length(xaxis.top.tck.lab));
            				}
        			args$top$ticks$at <- xat.top;
        			args$top$labels$at <- xat.top;
        			args$top$labels$labels <- xaxis.top.tck.lab;
				}
			}
        	return(args);
        	}
	xscale.components.old <- function(...) {
		args <- xscale.components.default(...);
		return(args);
		}
	xscale.list <- list(xscale.components.old, xscale.components.old, xscale.components.new);
	trellis.object <- update(
		combined.plot.objects,
		relation = 'free',
		skip = layout.skip,
		between = list(y = y.spacing, x = x.spacing),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = x.scale
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = y.scale
				)
			),
		main =  BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				cex = main.cex,
				just = main.just,
				x = main.x,
				y = main.y
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.label,
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				cex = xlab.cex
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
				label = rev(ylab.label),
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				cex = ylab.cex
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd,
				col = if ('Nature' == style) {'transparent'} else ('black')
				),
			layout.heights = list(
				panel = rev(panel.heights),
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 1 },
				main.key.padding =  main.key.padding,
				key.top = 0.1,
				key.axis.padding = 0.1,
				axis.top = 0.7,
				axis.bottom = 1.0,
				axis.xlab.padding = xlab.to.xaxis.padding,
				xlab = 1,
				xlab.key.padding = xlab.key.padding,
				key.bottom = key.bottom.padding,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = key.left.padding,
				key.ylab.padding = 0.5,
				ylab = 1,
				ylab.axis.padding = ylab.padding,
				xlab.axis.padding = xlab.padding,
				axis.left = 1,
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = panel.widths,
				between = 1,
				axis.right = 1,
				axis.key.padding = 1,
				key.right = key.right.padding,
				right.padding = right.padding
				),
			axis.components = list(left = list(pad1 = y.tck.dist), bottom = list(pad1 = x.tck.dist))

			),
		key = key,
		legend = if (print.new.legend) {legend} else {combined.plot.objects$legend}
		);
	if (!is.null(xaxis.top.tck.lab)) {
		trellis.object <- update(trellis.object, xscale.components = xscale.components.new);
		}
	# update above doesn't seem to go through so force it here
	trellis.object$x.limits <- xlimits;
	trellis.object$y.limits <- ylimits;
	if (!is.null(get.dendrogram.from)) {

		if (print.new.legend) {
			trellis.object$legend <- legend;
			}
		old.legend.top <- plot.objects[[get.dendrogram.from]]$legend$top$fun;


		if (!is.null(old.legend.top)) {
			if (is.null(trellis.object$legend$top$fun) || print.new.legend == FALSE) {
				trellis.object$legend$top$fun <- old.legend.top;
				trellis.object$legend$top$fun$framevp$width <- unit(dendrogram.top.size, 'npc');
				trellis.object$legend$top$fun$framevp$height <- unit(dendrogram.top.size, 'npc');
				trellis.object$legend$top$fun$framevp$x <- unit(dendrogram.top.x, 'points');
				trellis.object$legend$top$fun$framevp$y <- unit(dendrogram.top.y, 'points');
				}
			else {
				old.legend.top.height.cm <- convertUnit(
					grobHeight(old.legend.top),
					unitTo = 'cm',
					axisFrom = 'y',
					typeFrom = 'dimension',
					valueOnly = TRUE
					);

				old.legend.top$framevp$width <- unit(dendrogram.top.size, 'npc');
				old.legend.top$framevp$height <- unit(dendrogram.top.size, 'npc');
				old.legend.top$framevp$x <- unit(dendrogram.top.x, 'points');
				old.legend.top$framevp$y <- unit(dendrogram.top.y, 'points');

				top.layout.final <- grid.layout(
					ncol = 1,
					nrow = 2,
					heights = unit(
						x = c(old.legend.top.height.cm + 2, 1),
						units = c('cm', 'grobheight'),
						data = list(NULL, old.legend.top)
						),
					widths = unit(1, 'null'),
					respect = FALSE
					);

				# create a frame using this layout
				top.grob.final <- frameGrob(layout = top.layout.final);

				# place the existing grob
				top.grob.final <- placeGrob(
					frame = top.grob.final,
					grob = old.legend.top,
					row = 1,
					col = 1
					);

				# place the legend
				top.grob.final <- placeGrob(
					frame = top.grob.final,
					grob = trellis.object$legend$top$fun,
					row = 2,
					col = 1
					);

				trellis.object$legend$top$fun <- top.grob.final;
				}
			}

		old.legend.right <- plot.objects[[get.dendrogram.from]]$legend$right$fun;
		if (!is.null(old.legend.right)) {
			if (is.null(trellis.object$legend$right$fun)  || print.new.legend == FALSE) {
				trellis.object$legend$right$fun <- old.legend.right;
				trellis.object$legend$right$fun$framevp$width <- unit(dendrogram.right.size, 'npc');
				trellis.object$legend$right$fun$framevp$height <- unit(dendrogram.right.size, 'npc');
				trellis.object$legend$right$fun$framevp$x <- unit(dendrogram.right.x, 'points');
				trellis.object$legend$right$fun$framevp$y <- unit(dendrogram.right.y, 'points');
				}
			else {
				old.legend.right.width.cm <- convertUnit(
					grobWidth(old.legend.right),
					unitTo = 'cm',
					axisFrom = 'x',
					typeFrom = 'dimension',
					valueOnly = TRUE
					);

				old.legend.right$framevp$width <- unit(dendrogram.right.size, 'npc');
				old.legend.right$framevp$height <- unit(dendrogram.right.size, 'npc');
				old.legend.right$framevp$x <- unit(dendrogram.right.x, 'points');
				old.legend.right$framevp$y <- unit(dendrogram.right.y, 'points');




				right.layout.final <- grid.layout(
					nrow = 1,
					ncol = 2,
					widths = unit(
						x = c(1, old.legend.right.width.cm + 2),
						units = c('grobwidth', 'cm'),
						data = list(old.legend.right, NULL)
						),
					heights = unit(1, 'null'),
					respect = FALSE
					);

				right.grob.final <- frameGrob(layout = right.layout.final);

				# place the existing grob
				right.grob.final <- placeGrob(
					frame = right.grob.final,
					grob = old.legend.right,
					row = 1,
					col = 1
					);

				# place the legend
				right.grob.final <- placeGrob(
					frame = right.grob.final,
					grob = trellis.object$legend$right$fun,
					row = 1,
					col = 2
					);
				trellis.object$legend$right$fun <- right.grob.final;
				}
			}
		}
	# pulling forward a combination of axis limits, at and labels from the individual plots
	# and the values passed to multiplot as a argument to the created mutliplot
	if (!is.null(plot.labels.to.retrieve)) {
		nxaxis.labels <- list();
		nyaxis.labels <- list();
		nxat <- list();
		nyat <- list();
		nxlimits <- list();
		nylimits <- list();

		for (p in c(1:length(plot.objects))) {
			# if the plot is listed in plot.labels.to.retrieve, use the values from the individual plots
			if (p %in% plot.labels.to.retrieve) {
				nxlimits[[p]] <- plot.objects[[p]]$x.limits;
				nxat[[p]] <- plot.objects[[p]]$x.scales$at;
				nylimits[[p]] <- plot.objects[[p]]$y.limits;
				nyat[[p]] <- plot.objects[[p]]$y.scales$at;
				if (length(plot.objects[[p]]$x.scales$labels) > 0) {
					nxaxis.labels[[p]] <- plot.objects[[p]]$x.scales$labels;
					}
				else {
					nxaxis.labels[[p]] <- TRUE;
					}
				if (length(plot.objects[[p]]$y.scales$labels) > 0) {
					nyaxis.labels[[p]] <- plot.objects[[p]]$y.scales$labels;
					}
				else {
					nyaxis.labels[[p]] <- TRUE;
					}
				}
			# if the plot is not listed in plot.labels.to.retrieve, use the values pass to multiplot as arguments
			else {
				nxlimits[[p]] <- xlimits[[p]];
				nxat[[p]] <- xat[[p]];
				nylimits[[p]] <- ylimits[[p]];
				nyat[[p]] <- yat[[p]];
				if (length(xaxis.labels) >= p) {
					nxaxis.labels[[p]] <- xaxis.labels[[p]];
					}
				else {
					nxaxis.labels[[p]] <- TRUE;
					}
				if (length(yaxis.labels) >= p) {
					nyaxis.labels[[p]] <- yaxis.labels[[p]];
					}
				else {
					nyaxis.labels[[p]] <- TRUE;
					}
				}

			}
		trellis.object$x.limits <- nxlimits;
		trellis.object$y.limits <- nylimits;
		trellis.object$x.scales$at <- nxat;
		trellis.object$y.scales$at <- nyat;
		trellis.object$x.scales$labels <- nxaxis.labels;
		trellis.object$y.scales$labels <- nyaxis.labels;
		}

	# There is a glitch in update.trellis that prevents us from declaring multiple 'inside' legends
	# To get around this, we'll add in a special case to just set the 'legend' manually
	if (sum(names(legend) == 'inside', na.rm = TRUE) > 1) {
		trellis.object$legend <- legend;
		}

	# If flag set to TRUE for removing all border lines, reset panel border lines
	# TODO: RSUN, allow custom setting to redraw certain border lines
	if (remove.all.border.lines) {
       		trellis.object <- update(
       			trellis.object,
       			reference = FALSE,
       			par.settings = list(
				axis.line = list(
					col = 0,
					scales = list(col = 0, tck = c(0, 0)),
 					panel = function(...) {
 						lims <- current.panel.limits();
						panel.abline(h = lims$ylim[1], v = lims$xlim[1], col = 0);
						}
					)
				)
			);
       		}

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Re-add bottom and left axes
		trellis.object$axis <- function(side, line.col = 'black', ...) {
			# Only draw axes on the left and bottom
			if (side %in% c('bottom', 'left')) {
				axis.default(side = side, line.col = 'black', ...);
				lims <- current.panel.limits();
				panel.abline(h = lims$ylim[1], v = lims$xlim[1]);
				}
			}

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

### FUNCTION TO REPLACE ANY NULL VALUES IN XLIMITS OR YLIMITS ######################################
replace.nulls <- function(limits, carry.over, limits.plots, relation) {
	if (is.null(limits)) {
		limits <-
			if ('same' == relation) {carry.over}
			else {limits.plots};
 		}
 	else if ('same' == relation) {
 		limits <- carry.over;
 		}
 	else {
 		for (i in 1:length(limits)) {
 			if (is.null(limits[[i]])) {
 				limits[[i]] <- limits.plots[[i]];
 				}
 			}
 		}
 	return(limits);
	}
