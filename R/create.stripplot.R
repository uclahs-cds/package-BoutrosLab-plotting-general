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

### FUNCTION TO CREATE STRIPPLOTS #################################################################
create.stripplot <- function(
	formula, data, filename = NULL, groups = NULL, jitter.data = FALSE, jitter.factor = 1, jitter.amount = NULL,
	main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3,
	xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL,
	xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0,
	xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.col = 'black',
	yaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.rot = 0, yaxis.rot = 0,
	xaxis.tck = 0, yaxis.tck = 1, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, lwd = 1,
	pch = 19, col = 'black', col.border = 'black', fill = 'transparent', colour.alpha = 1, cex = 0.75,
	top.padding = 0.1, bottom.padding = 0.7, right.padding = 0.3, left.padding = 0.5, ylab.axis.padding = 1,
	layout = NULL, as.table = TRUE, x.spacing = 0, y.spacing = 0, add.median = FALSE, median.values = NULL,
	add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL,
	ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, strip.col = 'white', strip.cex = 1,
	strip.fontface = 'bold', key = NULL, legend = NULL, height = 6, width = 6, size.units = 'in',
	resolution = 1600, enable.warnings = FALSE, description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, inside.legend.auto = FALSE,
        disable.factor.sorting = FALSE 
	) {

	### store data on mount
        tryCatch({
                        dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if (!dir.exists(dir.name)) {
                                dir.create(dir.name);
                                }                        
			funcname <- 'create.stripplot';
                        print.to.file(dir.name, funcname, data, filename);
                        },
                warning = function(w) {
                        },
                error = function(e) {
                	});

	### needed to copy in case using variable to define rectangles dimensions
        rectangle.info <- list(
        	xright = xright.rectangle,
                xleft = xleft.rectangle,
                ytop = ytop.rectangle,
                ybottom = ybottom.rectangle
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

	# update groups function
	groups.new <- eval(substitute(groups), data, parent.frame());

	# Now make the actual plot object
	trellis.object <- lattice::stripplot(
		formula,
		data,
		panel = function(groups.local = groups.new, subscripts, ...) {

			# add rectangle if requested
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

			# make the stripplot
			panel.stripplot(
				jitter.data = jitter.data,
				factor = jitter.factor,
				amount = jitter.amount,
				groups = groups.new,
				subscripts = subscripts,
				...
				);

			# if requested, add lines indicating the group median
			if (add.median && is.null(median.values)) {
				warning('median.values must be specified to median to be added.');
				}
			else if (add.median && !is.null(median.values)) {
				meds <- median.values;
				xlocs <- seq_along(meds);
				panel.segments(
					xlocs - 1 / 4, meds, xlocs + 1 / 4, meds,
					lwd = 2,
					col = 'red'
					);
				}
			},
		type = 'p',
		cex = cex,
		pch = pch,
		col = mapply(
			function(pch, spot.colours, spot.border) {
				if (pch %in% 0:20) { return(spot.colours); } else if (pch %in% 21:25) { return(spot.border); }
				},
			pch,
			spot.colours = col,
			spot.border = col.border
			),
		fill = mapply(
			function(pch, spot.colours) {
				if (pch %in% 0:20) { NA; } else if (pch %in% 21:25) { return(spot.colours); }
				},
			pch,
			spot.colours = col
			),
		alpha = colour.alpha,
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
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
					alternating = FALSE,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					limits = ylimits,
					at = yat,
					tck = yaxis.tck,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
					alternating = FALSE,
					rot = yaxis.rot
					)
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
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
				axis.top = 1,
				axis.bottom = 1,
				axis.xlab.padding = 1,
				xlab = 1,
				xlab.key.padding = 0.5,
				key.bottom = 0.1,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 0.1,
				key.ylab.padding = 0.1,
				ylab = 1,
				ylab.axis.padding = ylab.axis.padding,
				axis.left = 1,
				axis.right = 1,
				axis.key.padding = 0.1,
				key.right = 1,
				right.padding = right.padding
				),
			strip.background = list(
				col = strip.col
				)
			),
		par.strip.text = list(
			cex = strip.cex,
			fontface = strip.fontface
			),
		layout = layout,
		as.table = as.table,
		key = key,
		legend = legend
		);

	if (disable.factor.sorting == TRUE) {

                sorting.param <- '';

                if (is.factor(trellis.object$panel.args[[1]][['y']])) {
                        sorting.param <- 'y';
                        if (is.null(trellis.object$y.scales$labels) || (is.logical(trellis.object$y.scales$labels[1]) && 
					trellis.object$y.scales$labels[1]  == TRUE)) {
                                default.labels <- unique(as.character(trellis.object$panel.args[[1]][[sorting.param]]));
                                trellis.object$y.scales$labels <- default.labels;
                                }
                        }
                else {
                        sorting.param <- 'x';
                        if (is.null(trellis.object$x.scales$labels) || (is.logical(trellis.object$x.scales$labels[1]) && 
					trellis.object$x.scales$labels[1]  == TRUE)) {
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
		extra.parameters <- list('x' = trellis.object$panel.args[[1]]$x, 'y' = trellis.object$panel.args[[1]]$y, 'ylimits' = trellis.object$y.limits,
			'xlimits' = trellis.object$x.limits, 'horizontal' = trellis.object$panel.args.common$horizontal);
		coords <- c();
		coords <- .inside.auto.legend('create.stripplot', filename, trellis.object, height, width, extra.parameters);
                trellis.object$legend$inside$x <- coords[1];
                trellis.object$legend$inside$y <- coords[2];
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
