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
	col.rectangle = 'transparent', alpha.rectangle = 1, box.ratio = 1, col = 'transparent',
	border.col = 'black', symbol.cex = 0.8, lwd = 1, outliers = TRUE, sample.order = 'none', order.by = 'median',
	xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2,
	xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, xlimits = NULL,
	ylimits = NULL,	xat = TRUE, yat = TRUE, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.cex = 1.5, yaxis.cex = 1.5,
	xaxis.col = 'black', yaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.rot = 0,
	yaxis.rot = 0, xaxis.tck = 1, yaxis.tck = 1, layout = NULL, as.table = FALSE, x.spacing = 0, y.spacing = 0,
	x.relation = 'same', y.relation = 'same', top.padding = 0.5, bottom.padding = 2, right.padding = 1,
	left.padding = 2, ylab.axis.padding = 0, add.text = FALSE, text.labels = NULL, text.x = NULL, text.y = NULL,
	text.anchor = 'centre', text.col = 'black', text.cex = 1, text.fontface = 'bold', add.pvalues = FALSE,
	pvalues.cex = c(1), key = NULL, legend = NULL, strip.col = 'white', strip.cex = 1, strip.fontface = 'bold',
	line.func = NULL, line.from = 0, line.to = 0, line.col = 'transparent', line.infront = TRUE,
	height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	use.legacy.settings = FALSE
	) {

	### needed to copy in case using variable to define rectangles dimensions
        rectangleInfo = list(
                                xright = xright.rectangle,
                                xleft = xleft.rectangle,
                                ytop = ytop.rectangle,
                                ybottom = ybottom.rectangle
                        );

        if(!is.null(yat) && length(yat) == 1){
        	if(yat == "auto"){
                	out = auto.axis(unlist(data[toString(formula[[2]])]))
                	data[toString(formula[[2]])] = out$x
			yat = out$at
                	yaxis.lab = out$axis.lab
		}

        	else if(yat == "auto.linear"){
                	out = auto.axis(unlist(data[toString(formula[[2]])]),log.scaled = FALSE)
                	data[toString(formula[[2]])] = out$x
                	yat = out$at
                	yaxis.lab = out$axis.lab
		}
    
        	else if(yat == "auto.log"){
                	out = auto.axis(unlist(data[toString(formula[[2]])]),log.scaled = TRUE)
                	data[toString(formula[[2]])] = out$x
                	yat = out$at
                	yaxis.lab = out$axis.lab
        	}
	}
	if(!is.null(xat) && length(xat) == 1){
        	if(xat == "auto"){
                	out = auto.axis(unlist(data[toString(formula[[3]])]))
                	data[toString(formula[[3]])] = out$x
                	xat = out$at
                	xaxis.lab = out$axis.lab
        	}
        	else if(xat == "auto.linear"){
                	out = auto.axis(unlist(data[toString(formula[[3]])]),log.scaled = FALSE)
                	data[toString(formula[[3]])] = out$x
                	xat = out$at
                	xaxis.lab = out$axis.lab
        	}
        	else if(xat == "auto.log"){
                	out = auto.axis(unlist(data[toString(formula[[3]])]),log.scaled = TRUE)
                	data[toString(formula[[3]])] = out$x
                	xat = out$at
                	xaxis.lab = out$axis.lab
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

	# if pvalues were requested
	pvalues <- list();
	positionpVal <- list();
	rotatePVals <- FALSE;

	if (add.pvalues) {
		col1 <- formula[3];
		col2 <- formula[2];
		mf <- model.frame(formula, as.data.frame(data));
		names <-  levels(mf[toString(col1)][[1]]);

		if (is.null(names)) {
			temp <- col1;
			col1 <- col2;
			col2 <- temp;
			names <- levels(mf[toString(col1)][[1]]);
			rotatePVals <- TRUE;
			}

 		factors <- as.vector(sapply(as.data.frame(mf)[toString(col1)], as.character));
		maxVal <- max(mf[toString(col2)][[1]]);

		for (i in 1:length(names)) {
			name <- toString(names[i]);
			d <- mf[toString(col2)][[1]][factors == name];
			t.value <- (mean(d) - 10) / (sd(d) / sqrt(length(d)));
			pvalues[i] <- round(2*pt(-abs(t.value), df=length(d)-1),4);

			if (pvalues[i] == 0) {
				pvalues[i] <- '<0.0001';
				}

			positionpVal[i] <- max(d) + maxVal * 0.03;
			}
		}

	# Now make the actual plot object
	trellis.object <- lattice::bwplot(
		x = formula,
		data,
		panel = function(...) {

			# add stripplot in background if requested
			if (add.stripplot) {

				panel.stripplot(
					jitter.data = TRUE,
					factor      = jitter.factor,
					amount      = jitter.amount,
					pch         = points.pch,
					col         = points.col,
					cex         = points.cex,
					alpha       = points.alpha,
					...
					);
				}

			# if requested add user defined rectangle
			if (add.rectangle) {

				panel.rect(
					xleft   = rectangleInfo$xleft,
					ybottom = rectangleInfo$ybottom,
					xright  = rectangleInfo$xright,
					ytop    = rectangleInfo$ytop,
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

				 panel.text(
					x        = text.x,
					y        = text.y,
					labels   = text.labels,
					col      = text.col,
					cex      = text.cex,
					fontface = text.fontface,
					adj      = text.anchor
					);
				}

			# Add pvalues if requested
			if (add.pvalues) {

				panel.text(
					x = if (rotatePVals) { positionpVal } else { c(1:length(pvalues)) },
					y = if (rotatePVals) { 1:length(pvalues) } else { positionpVal },
					labels = pvalues,
					col = 'black',
					cex = pvalues.cex,
					fontface = 'bold',
					srt = if (rotatePVals) { -90 } else { 0 }
					)
				}
			},
		fill = col,
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
			box.umbrella =list(
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
			if (sample.order == 'decreasing') { ranks <- rank(values.to.sort.by*(-1), ties.method = 'random'); }

			newlocations <- NULL;

			# create a list of the newlocations each box 'level' will appear
			for (i in c(1:length(num.boxes))) {
				newlocations[[i]] <- grep(num.boxes[i],trellis.object$panel.args[[1]]$y);
				}

			# replace the old values of the level with the new one based on rank
			for (i in c(1:length(num.boxes))) {
				trellis.object$panel.args[[1]]$y[newlocations[[i]]] = num.boxes[ranks[i]];
				}

			newPValues <- NULL;
			newPositionPVal <- NULL;

			# if pvalues were requested
			if (add.pvalues) {
				for (i in c(1:length(num.boxes))) {
					newPValues[ranks[i]] <- pvalues[i];
					newPositionPVal[ranks[i]]<- positionpVal[i];
		    			}

				pvalues <- newPValues;
				positionpVal <- newPositionPVal;
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
				warning("WARNING: the label order you specified has been reordered.");
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

			if (sample.order == 'decreasing') { ranks <- rank(values.to.sort.by*(-1), ties.method = 'random'); }

			newlocations <- NULL;

			for (i in c(1:length(num.boxes))) {
				newlocations[[i]] <- grep(num.boxes[i],trellis.object$panel.args[[1]]$x);
				}

			for (i in c(1:length(num.boxes))) {
				trellis.object$panel.args[[1]]$x[newlocations[[i]]] <- num.boxes[ranks[i]];
				}

			newPValues <- NULL;
			newPositionPVal <- NULL;

			# if pvalues were requested
			if (add.pvalues) {
				for (i in c(1:length(num.boxes))) {
					newPValues[ranks[i]] <- pvalues[i];
					newPositionPVal[ranks[i]] <- positionpVal[i];
					}

				pvalues <- newPValues;
				positionpVal <- newPositionPVal;
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
				warning("WARNING: the label order you specified has been reordered.");
				}
			}
		}

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Re-add bottom and left axes
		trellis.object$axis = function(side, line.col = 'black', ...) {
			# Only draw axes on the left and bottom
			if (side %in% c('bottom','left')) {
				axis.default(side = side, line.col = 'black', ...);
				lims <- current.panel.limits();
				panel.abline(h = lims$ylim[1], v = lims$xlim[1]);
				}
			}

		# Ensure sufficient resolution for graphs
		if (resolution < 1200) {
			resolution <- 1200;
			warning("Setting resolution to 1200 dpi.");
			}

		# Other required changes which are not accomplished here
		warning("Nature also requires italicized single-letter variables and en-dashes for ranges and negatives. See example in documentation for how to do this.");

		warning("Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend.");
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
