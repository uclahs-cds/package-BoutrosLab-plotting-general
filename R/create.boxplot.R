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

### FUNCTION TO CREATE BOXPLOTS ###################################################################
create.boxplot <- function(
	formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5,
	main.cex = 3, add.stripplot = FALSE, jitter.factor = 1, jitter.amount = NULL, points.pch = 19,
	points.col = 'darkgrey', points.cex = 0.5, points.alpha = 1, abline.h = NULL, abline.v = NULL,
	abline.lty = NULL, abline.lwd = NULL, abline.col = 'black', add.rectangle = FALSE,
	xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL,
	col.rectangle = 'transparent', alpha.rectangle = 1, box.ratio = 1, col = 'transparent', alpha = 1,
	border.col = 'black', symbol.cex = 0.8, lwd = 1, outliers = TRUE, sample.order = 'none', order.by = 'median',
	xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2,
	xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, xlimits = NULL,
	ylimits = NULL,	xat = TRUE, yat = TRUE, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.cex = 1.5, yaxis.cex = 1.5,
	xaxis.col = 'black', yaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.rot = 0,
	yaxis.rot = 0, xaxis.tck = c(1, 0), yaxis.tck = 1, layout = NULL, as.table = FALSE, x.spacing = 0, y.spacing = 0,
	x.relation = 'same', y.relation = 'same', top.padding = 0.5, bottom.padding = 2, right.padding = 1,
	left.padding = 2, ylab.axis.padding = 0, add.text = FALSE, text.labels = NULL, text.x = NULL, text.y = NULL,
	text.anchor = 'centre', text.col = 'black', text.cex = 1, text.fontface = 'bold',
	key = NULL, legend = NULL, strip.col = 'white', strip.cex = 1, strip.fontface = 'bold',
	line.func = NULL, line.from = 0, line.to = 0, line.col = 'transparent', line.infront = TRUE,
	height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	use.legacy.settings = FALSE, disable.factor.sorting = FALSE
	) {

	### needed to copy in case using variable to define rectangles dimensions -- wont carry through after change
	### store data on mount
	tryCatch({
			dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if (!dir.exists(dir.name)) {
				dir.create(dir.name);
				}
			funcname <- 'create.boxplot';
			print.to.file(dir.name, funcname, data, filename);

			## save all the parameters input (minus data) for later use
			param.data.filename <- paste(dir.name, 'data.RData', sep = '/Boxplot-');
			data.list.boxplot <- list();
			if (file.exists(param.data.filename)) {
				load(param.data.filename);
				}
			data.list.boxplot[[length(data.list.boxplot) + 1]] <- list(
				formula = formula, filename = filename, main = main, main.just = main.just, main.x = main.x, main.y = main.y, main.cex = main.cex,
				add.stripplot = add.stripplot, jitter.factor = jitter.factor, jitter.amount = jitter.amount, points.pch = points.pch,
				points.col = points.col, points.cex = points.cex, points.alpha = points.alpha, abline.h = abline.h, abline.v = abline.v,
				adline.lty = abline.lty, abline.lwd = abline.lwd, abline.col = abline.col, add.rectangle = add.rectangle,
				xleft.rectangle = xleft.rectangle, ybottom.rectangle = ybottom.rectangle, xright.rectangle = xright.rectangle,
				ytop.rectangle = ytop.rectangle, col.rectangle = col.rectangle, alpha.rectangle = alpha.rectangle, box.ratio = box.ratio, col = col,
				border.col = border.col, symbol.cex = symbol.cex, lwd = lwd, outliers = outliers, sample.order = sample.order, order.by = order.by,
				xlab.label = xlab.label, ylab.label = ylab.label, xlab.cex = xlab.cex, ylab.cex = ylab.cex, xlab.col = xlab.col, ylab.col = ylab.col,
				xlab.top.label = xlab.top.label, xlab.top.cex = xlab.top.cex, xlab.top.col = xlab.top.col, xlab.top.just = xlab.top.just,
				xlab.top.just = xlab.top.just, xlab.top.x = xlab.top.x, xlab.top.y = xlab.top.y, xlimits = xlimits, ylimits = ylimits, xat = xat,
				yat = yat, xaxis.lab = xaxis.lab, yaxis.lab = yaxis.lab, xaxis.cex = xaxis.cex, xaxis.col = xaxis.col, yaxis.col = yaxis.col,
				xaxis.fontface = xaxis.fontface, yaxis.fontface = yaxis.fontface, xaxis.rot = xaxis.rot, yaxis.rot = yaxis.rot, xaxis.tck = xaxis.tck,
				yaxis.tck = yaxis.tck, layout = layout, as.table = as.table, x.spacing = x.spacing, y.spacing = y.spacing, x.relation = x.relation,
				y.relation = y.relation, top.padding = top.padding, bottom.padding = bottom.padding, right.padding = right.padding,
				left.padding = left.padding, ylab.axis.padding = ylab.axis.padding, add.text = add.text, text.labels = text.labels, text.x = text.x,
				text.y = text.y, text.anchor = text.anchor, text.col = text.col, text.cex = text.cex, text.fontface = text.fontface, key = key,
				legend = legend, strip.col = strip.col, strip.cex = strip.cex, strip.fontface = strip.fontface, line.func = line.func,
				line.from = line.from, line.to = line.to, line.col = line.col, line.infront = line.infront, height = height, width = width,
				size.units = size.units, resolution = resolution
				);
			save(data.list.boxplot, file = param.data.filename);
			},
		warning = function(w) {
			},
		error = function(e) {
			}
		);

    parsed.formula <- unlist(strsplit(deparse(formula), ' [~|] '));
    formula.is.split <- '|' %in% all.names(formula);

	rectangle.info <- list(
		xright = xright.rectangle,
		xleft = xleft.rectangle,
		ytop = ytop.rectangle,
		ybottom = ybottom.rectangle
		);

	points.info <- list(
		pch = points.pch,
		col = points.col,
		cex = points.cex,
		alpha = points.alpha,
		groups = if (formula.is.split) { data[, parsed.formula[2]]; } else { NULL; }
		);

	text.info <- list(
		labels = text.labels,
		x = text.x,
		y = text.y,
		anchor = text.anchor,
		col = text.col,
		cex = text.cex,
		fontface = text.fontface
		);

	if (!is.null(yat) && length(yat) == 1) {
		if (yat == 'auto') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]));
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
			}

		else if (yat == 'auto.linear') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]), log.scaled = FALSE);
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
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
			out <- auto.axis(unlist(data[toString(formula[[3]])]));
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		else if (xat == 'auto.linear') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = FALSE);
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		else if (xat == 'auto.log') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = TRUE);
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		}

	# add preloaded defaults
	if (preload.default == 'paper') {
		}
	else if (preload.default == 'web') {
		}

	# parameter check
	if (!is.numeric(text.anchor) && !(tolower(text.anchor) %in% c('centre', 'center', 'left', 'right'))) {
		stop("Argument 'text.anchor' must be either numeric or one of 'left', 'right', and 'centre'.");
		}

	# Sort out text.anchor parameter
	# if left/right aligned, set text parameter adj to 0/1.
	if ('centre' == tolower(text.anchor) || 'center' == tolower(text.anchor)) {
		text.anchor <- 0.5;
		}
	else if ('left' == tolower(text.anchor)) {
		text.anchor <- 0;
		}
	else if ('right' == tolower(text.anchor)) {
		text.anchor <- 1;
		}

	# add stripplot if requested
	if (add.stripplot & outliers) {
		outliers <- FALSE;
		}

	# check class of conditioning variable
	if (formula.is.split) {
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
	trellis.object <- lattice::bwplot(
		x = formula,
		data,
		panel = function(subscripts, ...) {

			# add stripplot in background if requested
			if (add.stripplot) {
				panel.stripplot(
					jitter.data = TRUE,
					factor = jitter.factor,
					amount = jitter.amount,
					pch	 = points.info$pch,
					col	 = points.info$col,
					groups = points.info$groups,
					subscripts = if (!is.null(points.info$groups)) { subscripts; } else { NULL; },
					cex	 = points.info$cex,
					alpha  = points.info$alpha,
					...
					);
				}

			# if requested add user defined rectangle
			if (add.rectangle) {

				panel.rect(
					xleft   = rectangle.info$xleft,
					ybottom = rectangle.info$ybottom,
					xright  = rectangle.info$xright,
					ytop    = rectangle.info$ytop,
					col     = col.rectangle,
					alpha   = alpha.rectangle,
					border  = NA
					);
				}

			# create box plot
			panel.bwplot(pch = '|', col = 'black', ...);

			# add line if requested (behind)
			if (length(line.func) > 0 && line.infront == FALSE) {
				panel.curve(expr = line.func, from = line.from, to = line.to, col = line.col);
				}

			# add ablines
			panel.abline(h = abline.h, lty = abline.lty, lwd = abline.lwd, col = abline.col);
			panel.abline(v = abline.v, lty = abline.lty, lwd = abline.lwd, col = abline.col);

			# else add line in front if requested
			if (length(line.func) > 0 && line.infront == TRUE) {
				panel.curve(expr = line.func, from = line.from, to = line.to, col = line.col);
				}

			# Add text to plot
			if (add.text) {
				which.packet <- parent.frame(2)$which.packet;
				panel.text(
					x	= text.info$x,
					y	= text.info$y,
					labels   = text.info$labels[which.packet],
					col      = text.info$col,
					cex      = text.info$cex,
					fontface = text.info$fontface,
					adj      = text.info$anchor
					);
				}

			# Add pvalues if requested
			},
		fill = grDevices::adjustcolor(col, alpha),
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
				cex = main.cex,
				adj = 0,
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
				cex = xlab.cex,
				col = xlab.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' }
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
				cex = ylab.cex,
				col = ylab.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' }
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					rot = xaxis.rot,
					limits = xlimits,
					cex = xaxis.cex,
					col = xaxis.col,
					at = xat,
					relation = x.relation,
					tck = xaxis.tck,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface }
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					rot = yaxis.rot,
					limits = ylimits,
					cex = yaxis.cex,
					col = yaxis.col,
					tck = yaxis.tck,
					at = yat,
					relation = y.relation,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface }
					)
				),
			alternating = 1
			),
		par.settings = list(
			axis.line = list(
				lwd = lwd,
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
				axis.xlab.padding = 0.1,
				xlab = 1,
				xlab.key.padding = 0.5,
				key.bottom = 0.1,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 0,
				key.ylab.padding = 0.3,
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
			box.dot = list(
				pch = 19,
				col = border.col,
				lty = 1
				),
			box.rectangle = list(
				lwd = lwd,
				col = border.col,
				lty = 1
				),
			box.umbrella = list(
				lwd = lwd,
				col = border.col,
				lty = 1
				),
			plot.symbol = list(
				col = border.col,
				pch = 19,
				cex = symbol.cex
				),
			strip.background = list(
				col = strip.col
				)
			),
		par.strip.text = list(
			cex = strip.cex,
			fontface = strip.fontface
			),
		do.out = outliers,
		layout = layout,
		as.table = as.table,
		pretty = TRUE,
		key = key,
		legend = legend,
		box.ratio = box.ratio
		);

	if (disable.factor.sorting == TRUE) {

		sorting.param <- '';

		if (is.factor(trellis.object$panel.args[[1]][['y']])) {
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

	# reorder by median
	if (sample.order == 'increasing' | sample.order == 'decreasing') {

		if (is.numeric(trellis.object$panel.args[[1]]$x)) {
			num.boxes <- levels(trellis.object$panel.args[[1]]$y);
			values.to.sort.by <- NULL;

			# create a list of values to sort by for each box
			for (i in c(1:length(num.boxes))) {
				if (order.by == 'median') {
					values.to.sort.by[i] <- median(trellis.object$panel.args[[1]]$x[trellis.object$panel.args[[1]]$y == num.boxes[[i]]]);
					}
				else if (order.by == 'mean') {
					values.to.sort.by[i] <- mean(trellis.object$panel.args[[1]]$x[trellis.object$panel.args[[1]]$y == num.boxes[[i]]]);
					}
				else if (order.by == 'min') {
					values.to.sort.by[i] <- min(trellis.object$panel.args[[1]]$x[trellis.object$panel.args[[1]]$y == num.boxes[[i]]]);
					}
				else if (order.by == 'max') {
					values.to.sort.by[i] <- max(trellis.object$panel.args[[1]]$x[trellis.object$panel.args[[1]]$y == num.boxes[[i]]]);
					}
				}

			ranks <- rank(values.to.sort.by, ties.method = 'random');

			# swap the rankings if decreasing order is specified
			if (sample.order == 'decreasing') { ranks <- rank(values.to.sort.by * ( -1 ), ties.method = 'random'); }

			newlocations <- NULL;

			# create a list of the newlocations each box 'level' will appear
			for (i in c(1:length(num.boxes))) {
				newlocations[[i]] <- grep(num.boxes[i], trellis.object$panel.args[[1]]$y);
				}

			# replace the old values of the level with the new one based on rank
			for (i in c(1:length(num.boxes))) {
				trellis.object$panel.args[[1]]$y[newlocations[[i]]] <- num.boxes[ranks[i]];
				}



			# if labels were not specified reorder the default ones
			if (length(yaxis.lab) == 1 && yaxis.lab) {
				for (i in c(1:length(num.boxes))) {
					trellis.object$y.scales$labels[ranks[i]] <- num.boxes[i];
					}
				}
			else {
				newlabels <- NULL;
				for (i in c(1:length(num.boxes))) {
					newlabels[ranks[i]] <- trellis.object$y.scales$labels[i];
					}
				trellis.object$y.scales$labels <- newlabels;
				warning('WARNING: the label order you specified has been reordered.');
				}
			}
		else {
			num.boxes <- levels(trellis.object$panel.args[[1]]$x);
			values.to.sort.by <- NULL;

			# create a list of the values to sort by for each box
			for (i in c(1:length(num.boxes))) {
				if (order.by == 'median') {
					values.to.sort.by[i] <- median(trellis.object$panel.args[[1]]$y[trellis.object$panel.args[[1]]$x == num.boxes[[i]]]);
					}
				else if (order.by == 'mean') {
					values.to.sort.by[i] <- mean(trellis.object$panel.args[[1]]$y[trellis.object$panel.args[[1]]$x == num.boxes[[i]]]);
					}
				else if (order.by == 'min') {
					values.to.sort.by[i] <- min(trellis.object$panel.args[[1]]$y[trellis.object$panel.args[[1]]$x == num.boxes[[i]]]);
					}
				else if (order.by == 'max') {
					values.to.sort.by[i] <- max(trellis.object$panel.args[[1]]$y[trellis.object$panel.args[[1]]$x == num.boxes[[i]]]);
					}
				}

			ranks <- rank(values.to.sort.by, ties.method = 'random');

			if (sample.order == 'decreasing') { ranks <- rank(values.to.sort.by * (-1), ties.method = 'random'); }

			newlocations <- NULL;

			for (i in c(1:length(num.boxes))) {
				newlocations[[i]] <- grep(num.boxes[i], trellis.object$panel.args[[1]]$x);
				}

			for (i in c(1:length(num.boxes))) {
				trellis.object$panel.args[[1]]$x[newlocations[[i]]] <- num.boxes[ranks[i]];
				}


			if (length(xaxis.lab) == 1 && xaxis.lab) {
				for (i in c(1:length(num.boxes))) {
					trellis.object$x.scales$labels[ranks[i]] <- num.boxes[i];
					}
				}
			else {
				newlabels <- NULL;
				for (i in c(1:length(num.boxes))) {
					newlabels[ranks[i]] <- trellis.object$x.scales$labels[i];
					}
				trellis.object$x.scales$labels <- newlabels;
				warning('WARNING: the label order you specified has been reordered.');
				}
			}
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

		warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend.');
		}

	# Otherwise use the BL style if requested
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
