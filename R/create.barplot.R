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

### FUNCTION TO CREATE BARPLOTS ###################################################################
create.barplot <- function(
	formula, data, groups = NULL, stack = FALSE, filename = NULL, main = NULL, main.just = 'center',
	main.x = 0.5, main.y = 0.5, main.cex = 3, xlab.label = tail(sub('~', '', formula[-2]), 1),
	ylab.label = tail(sub('~', '', formula[-3]), 1), xlab.cex = 2, ylab.cex = 2, xlab.col = 'black',
	ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black',
	xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, abline.h = NULL, abline.v = NULL,
	abline.lty = 1, abline.lwd = NULL, abline.col = 'black', axes.lwd = 1, add.grid = FALSE, xgrid.at = xat,
	ygrid.at = yat, grid.lwd = 5, grid.col = NULL, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.col = 'black',
	yaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.cex = 1.5, yaxis.cex = 1.5,
	xaxis.rot = 0, yaxis.rot = 0, xaxis.tck = 1, yaxis.tck = 1, xlimits = NULL, ylimits = NULL, xat = TRUE,
	yat = TRUE, layout = NULL, as.table = FALSE, x.spacing = 0, y.spacing = 0, x.relation = 'same',
	y.relation = 'same', top.padding = 0.5, bottom.padding = 1, right.padding = 1, left.padding = 1,
	key.bottom = 0.1, ylab.axis.padding = 0.5, xlab.axis.padding = 0.5, col = 'black', border.col = 'black',
	border.lwd = 1, plot.horizontal = FALSE, background.col = 'transparent', origin = 0, reference = TRUE,
	box.ratio = 2, sample.order = 'none', group.labels = FALSE, key = list(text = list(lab = c(''))),
	legend = NULL, add.text = FALSE, text.labels = NULL, text.x = NULL, text.y = NULL, text.col = 'black',
	text.cex = 1, text.fontface = 'bold', strip.col = 'white', strip.cex = 1, y.error.up = NULL,
	y.error.down = y.error.up, y.error.bar.col = 'black', error.whisker.width = width / (nrow(data) * 4),
	error.bar.lwd = 1, error.whisker.angle = 90, add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL,
	xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'grey85', alpha.rectangle = 1,
	line.func = NULL, line.from = 0, line.to = 0, line.col = 'transparent', line.infront = TRUE,
	text.above.bars = list(labels = NULL, padding = NULL, bar.locations = NULL, rotation = 0),
	raster = NULL, raster.vert = TRUE, raster.just = 'center', raster.width.dim = unit(2 / 37, 'npc'),
	height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	use.legacy.settings = FALSE, inside.legend.auto = FALSE, disable.factor.sorting = FALSE
	) {

	### needed to copy in case using variable to define rectangles dimensions
	rectangle.info <- list(
		xright = xright.rectangle,
		xleft = xleft.rectangle,
		ytop = ytop.rectangle,
		ybottom = ybottom.rectangle
		);

        text.info <- list(
                labels = text.labels,
                x = text.x,
                y = text.y,
                col = text.col,
                cex = text.cex,
                fontface = text.fontface
                );

	if (!is.null(yat) && length(yat) == 1) {
		if (yat == 'auto') {
			if (stack == TRUE) {
				# run once to get data readjustment (in case log)
				s <- split(data, data[toString(formula[[3]])]);
				final.list <- list();
				for (x in 1:length(s)) {
					final.list[[x]] <- sum(s[[x]][toString(formula[[2]])]);
					}
				out <- auto.axis(final.list, log.scaled = FALSE);
				yat <- out$at;
				yaxis.lab <- out$axis.lab;
				}
			else {
				out <- auto.axis(unlist(data[toString(formula[[2]])]));
				data[toString(formula[[2]])] <- out$x;
				yat <- out$at;
				yaxis.lab <- out$axis.lab;
				}
			}

		else if (yat == 'auto.linear') {
			if (stack == TRUE) {
				# run once to get data readjustment (in case log)
				s <- split(data, data[toString(formula[[3]])]);
				final.list <- list();
				for (x in 1:length(s)) {
					final.list[[x]] <- sum(s[[x]][toString(formula[[2]])]);
					}
				out <- auto.axis(final.list, log.scaled = FALSE);
				yat <- out$at;
				yaxis.lab <- out$axis.lab;
				}
			else {
				out <- auto.axis(unlist(data[toString(formula[[2]])]), log.scaled = FALSE);
				data[toString(formula[[2]])] <- out$x;
				yat <- out$at;
				yaxis.lab <- out$axis.lab;
				}
			}

		else if (yat == 'auto.log') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]), log.scaled = TRUE);
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
			}
		}

	if (!is.null(xat) && length(xat) == 1) {
		if (xat == 'auto') {
			if (stack == TRUE) {
				# run once to get data readjustment (in case log)
				s <- split(data, data[toString(formula[[3]])]);
				final.list <- list();
				for (x in 1:length(s)) {
					final.list[[x]] <- sum(s[[x]][toString(formula[[2]])]);
					}
				out <- auto.axis(final.list, log.scaled = FALSE);
				xat <- out$at;
				xaxis.lab <- out$axis.lab;
				}
			else {
				out <- auto.axis(unlist(data[toString(formula[[3]])]));
				data[toString(formula[[3]])] <- out$x;
				xat <- out$at;
				xaxis.lab <- out$axis.lab;
				}
			}

		else if (xat == 'auto.linear') {
			if (stack == TRUE) {
				# run once to get data readjustment (in case log)
				s <- split(data, data[toString(formula[[3]])]);
				final.list <- list();
				for (x in 1:length(s)) {
					final.list[[x]] <- sum(s[[x]][toString(formula[[2]])]);
					}
				out <- auto.axis(final.list, log.scaled = FALSE);
				xat <- out$at;
				xaxis.lab <- out$axis.lab;
				}
			else {
				out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = FALSE);
				data[toString(formula[[3]])] <- out$x;
				xat <- out$at;
				xaxis.lab <- out$axis.lab;
				}
			}

		else if (xat == 'auto.log') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = TRUE);
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		}

	####### Error checking ########
	tryCatch(
		expr = {
			if (is.null(formula)) { stop(); }
			as.formula(formula);
			},
		error = function(message) {
			stop('Invalid formula.');
			}
		);

	# add preloaded defaults
	if (preload.default == 'paper') {
		}
	else if (preload.default == 'web') {
		}

	# allow a gray spectrum if groups is specified and only a single colour is given
	groups.new <- eval(substitute(groups), data, parent.frame());

	if (!is.null(groups.new) && 1 == length(col) && col == 'grey') {
		col <- grey(1:nlevels(as.factor(groups.new)) / nlevels(as.factor(groups.new)));
		}

	# check class of conditioning variable
	if ('|' %in% all.names(formula)) {
		variable <- sub('^\\s+', '', unlist(strsplit(toString(formula[length(formula)]), '\\|'))[2]);
		if (variable %in% names(data)) {
			cond.class <- class(data[, variable]);
			if (cond.class %in% c('integer', 'numeric')) {
				warning(
					'Numeric values detected for conditional variable. If text labels are desired, please convert conditional variable to character.'
					);
				}
			rm(cond.class);
			}
		}

	# Now make the actual plot object
	trellis.object <- lattice::barchart(
		formula,
		data,
		panel = function(x, y, subscripts, groups = groups.new, ...) {

			# add rectangle
			if (add.rectangle) {
				panel.rect(
					xleft = rectangle.info$xleft,
					ybottom = rectangle.info$ybottom,
					xright = rectangle.info$xright,
					ytop = rectangle.info$ytop,
					col = col.rectangle,
					alpha = alpha.rectangle,
					border = NA
					);
				}

			if (!is.null(text.above.bars$labels)) {
				if (!is.null(groups.new)) {
					stop("Argument 'text.above.bars' does not work with grouped plots.");
					}

				# Common arguments for both horizontal and vertical orientation
				text.above.bars.args <- list(
					labels = text.above.bars$labels,
					srt = text.above.bars$rotation
					);

				# Keep all additional arguments passed to text.above.bars
				# This is the equivalent of ... to an argument
				text.above.bars.args <- c(
					text.above.bars.args,
					text.above.bars[
						! names(text.above.bars) %in%
							c('labels', 'padding', 'bar.locations', 'rotation', 'srt')
						]
					);
				
				# change orientation if requested
				if (plot.horizontal) {
					text.above.bars.args$x <- x[text.above.bars$bar.locations] + text.above.bars$padding;
					text.above.bars.args$y <- text.above.bars$bar.locations;
					}
				else {
					text.above.bars.args$x <- text.above.bars$bar.locations;
					text.above.bars.args$y <- y[text.above.bars$bar.locations] + text.above.bars$padding;
					}
				
				do.call(panel.text, text.above.bars.args);
				}

			# add background shading
	#		if (add.background.shading) {
	#			if (!is.null(background.shading.xpos)) {
	#				if (length(background.shading.xpos)%%2 == 1) {
	#					xleft <- background.shading.xpos[seq(1,length(background.shading.xpos)-1,2)];
	#					xright <- background.shading.xpos[seq(2,length(background.shading.xpos)-1,2)];
	#					}
	#				else {
	#					xleft <- background.shading.xpos[seq(1,length(background.shading.xpos),2)];
	#					xright <- background.shading.xpos[seq(2,length(background.shading.xpos),2)];
	#					}
	#				}
	#			else {
	#				xleft <- xlimits[1];
	#				xright <- xlimits[2];
	#				}

	#			if (!is.null(background.shading.ypos)) {
	#				if (length(background.shading.ypos)%%2 == 1) {
	#					ybottom <- background.shading.ypos[seq(1,length(background.shading.ypos)-1,2)];
	#					ytop <- background.shading.ypos[seq(2,length(background.shading.ypos)-1,2)];
	#					}
	#				else {
	#					ybottom <- background.shading.ypos[seq(1,length(background.shading.ypos),2)];
	#					ytop <- background.shading.ypos[seq(2,length(background.shading.ypos),2)];
	#					}
	#				}

	#			else {
	#				ybottom <- ylimits[1];
	#				ytop <- ylimits[2];
	#				}

	#			panel.rect(
	#				xleft = xleft,
	#				ybottom = ybottom,
	#				xright = xright,
	#				ytop = ytop,
	#				col = background.shading.colour,
	#				border = 'transparent'
	#				);
	#			}

			# add grid-lines
			if (add.grid) {
				panel.abline(
					v = BoutrosLab.plotting.general::generate.at.final(
						at.input = xgrid.at,
						limits = xlimits,
						data.vector = data$x
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = data$y
						),
					col = if (is.null(grid.col)) { trellis.par.get('reference.line')$col } else { grid.col },
					lwd = grid.lwd,
					);
				}

			# Add the barplot
			panel.barchart(x, y, subscripts = subscripts, groups = groups.new, border = border.col, lwd = border.lwd, ..., origin = origin);

			# add lines superimposed
			if (length(line.func) > 0 && !line.infront) {
				panel.curve(expr = line.func, from = line.from, to = line.to, col = line.col);
				}

			panel.abline(h = abline.h, lty = abline.lty, lwd = abline.lwd, col = abline.col);
			panel.abline(v = abline.v, lty = abline.lty, lwd = abline.lwd, col = abline.col);

			if (length(line.func) > 0 && line.infront) {
				panel.curve(expr = line.func, from = line.from, to = line.to, col = line.col);
				}

			# Add text to plot
			if (add.text) {
				panel.text(
					x        = text.info$x,
					y        = text.info$y,
					labels   = text.info$labels,
					col      = text.info$col,
					cex      = text.info$cex,
					fontface = text.info$fontface
					);
				}

			# add error bars
			if (!is.null(y.error.up)) {

				# handle x-position offset due to groups
				if (!is.null(groups)) {

					num.groups <- length(subscripts) / length(unique(groups));
					group.num  <- (subscripts - 1) %/% num.groups;
					if (length(unique(group.num)) %% 2 == 1) {
						group.num <- group.num - trunc(length(unique(group.num)) / 2);
						}
					else {
						number.of.groups <- trunc(length(unique(group.num)) / 2);
						subtr <- 1 + 2 * (number.of.groups - 1);
						group.num <- group.num * 2 - subtr;
						}
					offset <- (6 / (nrow(data) * 4)) * (group.num) * (1.75 - (0.85 * (length(unique(groups)) + 1) %% 2));
					}
				else {
					offset <- 0;
					}
				if (!plot.horizontal) {
					panel.arrows(
						# convert to numeric to handle when x is a factor
						x0 = as.numeric(x) + offset,
						y0 = y + y.error.up,
						x1 = as.numeric(x) + offset,
						y1 = y - y.error.down,
						length = error.whisker.width,
						angle = error.whisker.angle,
						ends = 'both',
						col = y.error.bar.col,
						lwd = error.bar.lwd
						);
					}
				else {
					panel.arrows(
						# convert to numeric to handle when x is a factor
						y0 = as.numeric(y) + offset,
						x0 = x + y.error.up,
						y1 = as.numeric(y) + offset,
						x1 = x - y.error.down,
						length = error.whisker.width,
						angle = error.whisker.angle,
						ends = 'both',
						col = y.error.bar.col,
						lwd = error.bar.lwd
						);

					}
				}

			# add raster fill
			if (!is.null(raster)) {
				if (raster.vert) {
					grid.raster(
						raster,
						x = x,
						y = 0,
						height = y,
						just = raster.just,
						default.units = 'native',
						width = raster.width.dim
						);
					}
				else {
					grid.raster(
						raster,
						x = 0,
						width = x,
						y = y,
						just = raster.just,
						default.units = 'native',
						height = raster.width.dim
						);
					}
				}
			},
		horizontal = plot.horizontal,
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
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
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
				cex = xlab.cex,
				col = xlab.col
				)
			),
		xlab.top = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.top.label,
				cex = xlab.top.cex,
				col = xlab.top.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
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
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
				cex = ylab.cex,
				col = ylab.col
				)
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					limits = xlimits,
					at = xat,
					tck = xaxis.tck,
					relation = x.relation,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
					alternating = FALSE
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					rot = yaxis.rot,
					col = yaxis.col,
					limits = ylimits,
					at = yat,
					tck = yaxis.tck,
					relation = y.relation,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
					alternating = FALSE
					)
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
			),
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd,
				col = if ('Nature' == style) { 'transparent' } else { 'black' }
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 1 },
				main.key.padding = 0.1,
				key.top = 0.1,
				key.axis.padding = 0.1,
				axis.top = 0.7,
				axis.bottom = 1.0,
				axis.xlab.padding = xlab.axis.padding,
				xlab = 1,
				xlab.key.padding = 0.1,
				key.bottom = key.bottom,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 0,
				key.ylab.padding = 0.5,
				ylab = 1,
				ylab.axis.padding = ylab.axis.padding,
				axis.left = 1,
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = 1,
				between = 1,
				axis.right = 1,
				axis.key.padding = 1,
				key.right = 1,
				right.padding = right.padding
				),
			strip.background = list(
				col = strip.col
				),
			panel.background = list(
				col = background.col
				)
			),
		par.strip.text = list(
			cex = strip.cex
			),
		col = col,
		layout = layout,
		as.table = as.table,
		key = key,
		legend = legend,
		pretty = TRUE,
		stack = stack,
		reference = reference,
		box.ratio = box.ratio
		);

	if (disable.factor.sorting == TRUE) {

		sorting.param <- '';

		if (plot.horizontal) {
			sorting.param <- 'y';
			if (is.null(trellis.object$y.scales$labels) || (is.logical(trellis.object$y.scales$labels[1]) && trellis.object$y.scales$labels[1]  == TRUE)) {
				default.labels <- unique(as.character(trellis.object$panel.args[[1]][[sorting.param]]));
				trellis.object$y.scales$labels <- default.labels;
				}
			}
		else {
			sorting.param <- 'x';
			if (is.null(trellis.object$x.scales$labels) || (is.logical(trellis.object$x.scales$labels[1]) && trellis.object$x.scales$labels[1]  == TRUE)) {
                        	default.labels <- unique(as.character(trellis.object$panel.args[[1]][[sorting.param]]));
				trellis.object$x.scales$labels <- default.labels;
				}
			}

                unique.mapping <- list();
                count <- 1;
                for (x in trellis.object$panel.args[[1]][[sorting.param]]) {
                        if (is.null(unique.mapping[[as.character(x)]])) {
                                unique.mapping[as.character(x)] <- count;
                                count <- count + 1;
                                }
                        }
                temp.data <- as.character(trellis.object$panel.args[[1]][[sorting.param]]);
                for (x in 1:length(temp.data)) {
                        temp.data[x] <- as.character(unique.mapping[as.character(trellis.object$panel.args[[1]][[sorting.param]][[x]])][[1]]);
                        }
                trellis.object$panel.args[[1]][[sorting.param]] <- as.numeric(temp.data);

		}

	if (inside.legend.auto) {
		extra.parameters <- list('data' = data, 'plot.horizontal' = plot.horizontal, 'formula' = formula, 'groups' = groups,
			'stack' = stack, 'ylimits' = trellis.object$y.limits, 'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.barplot', filename, trellis.object, height, width, extra.parameters);
		trellis.object$legend$inside$x <- coords[1];
		trellis.object$legend$inside$y <- coords[2];
		}

	# add grouped labels
	if (group.labels) {
		num.groups <- length(trellis.object$panel.args[[1]]$x) / length(unique(trellis.object$panel.args[[1]]$x));
		intialaddition <- (1 / 3) / num.groups;
		additions <- intialaddition * 2;
		newxat <- NULL;

		if (is.logical(trellis.object$x.scales$at)) {
			trellis.object$x.scales$at <- c(1:length(unique(trellis.object$panel.args[[1]]$x)));
			}

		for (i in trellis.object$x.scales$at) {
			for (j in c(1:num.groups)) {
				newxat <- c(newxat, i - 1 / 3 + intialaddition + additions * (j - 1));
				}
			}

		trellis.object$x.scales$at <- newxat;
		}

	# reorder the bars in decreasing or increasing order if specified
	if (is.null(sample.order) || is.na(sample.order)) { sample.order <- 'none'; }

	if (sample.order[1] != 'none') {
		for (i in 1:length(trellis.object$panel.args)) {

			# will need two separate ways for horizontal and non - horizontal
			if (!plot.horizontal) {
				num.bars <- length(unique(trellis.object$panel.args[[1]]$x));

				if (length(unique(sample.order)) == num.bars) {
					if (length(xaxis.lab) == 1 && xaxis.lab) {
						ordering <- rev(match(sample.order, trellis.object$panel.args[[i]]$x));
						}
					else {
						ordering <- rev(match(sample.order, trellis.object$x.scales$labels));
						}
					}

				if (length(sample.order) == 1) {
					if(! sample.order %in% c('decreasing', 'increasing')) {
						stop('sample.order should be `decreasing` or `increasing`');
						}

					# This looks backwards but gets reversed later
					# Might want to revisit if it makes more sense to sort in correct order here
					sample.order.decreasing <- sample.order != 'decreasing';
					ordering <- order(
						trellis.object$panel.args[[1]]$y[c(1:num.bars)],
						decreasing = sample.order.decreasing
						);
					}

				# if label locations are specified, change them
				if (xat != TRUE) {
					newxat <- NULL;
					for (j in rev(ordering)) {
						if (length(which(xat == j) > 0)) {
							newxat <- c(newxat, which(rev(ordering) == j));
							}
						else { newxat <- c(newxat, 0); }
						}

					trellis.object$x.scales$at <- newxat;
					}

				# if labels were not specified reorder the default ones
				if (length(xaxis.lab) == 1 && xaxis.lab) {
					trellis.object$x.scales$labels <- rep(
						trellis.object$panel.args[[i]]$x[rev(ordering)],
						length(trellis.object$panel.args[[1]]$x) / num.bars
						);
					}

				# if labels are specified reorder the specified ones
				else {
					trellis.object$x.scales$labels <- rev(trellis.object$x.scales$labels[rev(ordering)]);
					warning('WARNING: the label order you specified has been reordered.');
					}

				for (j in 0:(length(trellis.object$panel.args[[1]]$x) / num.bars - 1)) {
					# reorder values of bars
					trellis.object$panel.args[[i]]$y[c( (1 + j * num.bars) : (num.bars * (j + 1) ) )] <- rev(
						trellis.object$panel.args[[i]]$y[ordering + num.bars * j]
						);

					# reorder values of x to order in logical order
					trellis.object$panel.args[[i]]$x <- rep(1:length(ordering), length(trellis.object$panel.args[[1]]$x) / num.bars);
					}
				}
			else {
				num.bars <- length(unique(trellis.object$panel.args[[1]]$y));

				if (length(unique(sample.order)) == num.bars) {
					if (length(yaxis.lab) == 1 && yaxis.lab) {
						ordering <- rev(match(sample.order, sort(sample.order)[trellis.object$panel.args[[i]]$y]));
						}
					else {
						ordering <- rev(match(sample.order, sort(sample.order)[trellis.object$y.scales$labels]));
						}
					}

				if (length(sample.order) == 1) {
					if(! sample.order %in% c('decreasing', 'increasing')) {
						stop('sample.order should be `decreasing` or `increasing`');
						}

					sample.order.decreasing <- sample.order != 'decreasing';
					ordering <- order(
						trellis.object$panel.args[[1]]$x[c(1:num.bars)],
						decreasing = sample.order.decreasing
						);
					}

				if (!yat) {
					newyat <- NULL;
					for (j in rev(ordering)) {
						if (length(which(yat == j) > 0)) {
							newyat <- c(newyat, which(rev(ordering) == j));
							}
						else {
							newyat <- c(newyat, 0);
							}
						}

					trellis.object$y.scales$at <- newyat;
					}

				if (length(yaxis.lab) == 1 && yaxis.lab) {
					trellis.object$y.scales$labels <- rep(
						trellis.object$panel.args[[i]]$y[ordering],
						length(trellis.object$panel.args[[1]]$y) / num.bars
						);
					}
				else {
					trellis.object$y.scales$labels <- rev(trellis.object$y.scales$labels[ordering]);
					warning('WARNING: the label order you specified has been reordered.');
					}

				for (j in 0:(length(trellis.object$panel.args[[1]]$y) / num.bars - 1)) {
					trellis.object$panel.args[[i]]$x[c( (1 + j * num.bars) : (num.bars * (j + 1) ) )] <-
						trellis.object$panel.args[[i]]$x[ordering + num.bars * j];

					trellis.object$panel.args[[i]]$y <- rep(1:length(ordering), length(trellis.object$panel.args[[1]]$y) / num.bars);
					}
				}
			}

		y.error.up <- y.error.up[rev(ordering)];
		y.error.down <- y.error.down[rev(ordering)];
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
		warning('Nature also requires italicized single-letter variables and
			en-dashes for ranges and negatives. See example in documentation for how to do this.');

		warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend.');
		}

	# Otherwise use the BL style if requested
	else if ('BoutrosLab' == style) {
		# Nothing happens
		}

	# if neither of the above is requested, give a warning
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
