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

### FUNCTION TO CREATE MANHATTANPLOTS ##############################################################
create.manhattanplot <- function(
	formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5,
	main.cex = 3, xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2,
	xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, xlimits = NULL,
	ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA, xaxis.log = FALSE, yaxis.log = FALSE,
	xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.rot = 0, yaxis.rot = 0, xaxis.fontface = 'plain', yaxis.fontface = 'plain',
	xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 0, yaxis.tck = c(1, 1), horizontal = FALSE, type = 'p',
	cex = 2, pch = '.', col = 'black', lwd = 1, lty = 1, alpha = 1, strip.col = 'white', strip.cex = 1,
	axes.lwd = 1, axes.lty = 'dashed', key = list(text = list(lab = c(''))), legend = NULL, layout = NULL,
	as.table = FALSE, x.spacing = 0, y.spacing = 0, x.relation = 'same', y.relation = 'same', top.padding = 0,
	bottom.padding = 0, right.padding = 0, left.padding = 0, key.top = 0, key.left.padding = 0, ylab.axis.padding = 1,
	axis.key.padding = 1, abline.h = NULL, abline.col = 'black', abline.lwd = 1, abline.lty = 1, add.rectangle = FALSE,
	xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL,
	col.rectangle = 'transparent', alpha.rectangle = 1, add.points = FALSE, points.x = NULL, points.y = NULL,
	points.pch = 19, points.col = 'black', points.cex = 1, add.text = FALSE, text.labels = NULL, text.x = NULL,
	text.y = NULL, text.col = 'black', text.cex = 1, text.fontface = 'bold', height = 6, width = 10, size.units = 'in',
	resolution = 1600, enable.warnings = FALSE, style = 'BoutrosLab',
	description = 'Created with BoutrosLab.plotting.general', preload.default = 'custom', use.legacy.settings = FALSE,
	inside.legend.auto = FALSE, ...
	) {

	### store data on mount
        tryCatch({
                        dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if( !dir.exists(dir.name) ) {
                                dir.create(dir.name);
                                }

			funcname <- 'create.manhattanplot';
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

	# Now make the actual plot object
	trellis.object <- lattice::xyplot(
		formula,
		data,
		panel = function(subscripts, type.local = type, abline.local = abline, ...) {

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

			# if requested, add user-defined horizontal line
			if (!is.null(abline.h)) {
				panel.abline(
					h   = abline.h,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
					);
				}

			# create the main plot
			panel.xyplot(
				subscripts = subscripts,
				type = setdiff(type.local, 'g'),
				alpha = alpha,
				horizontal = horizontal,
				...
				);

						# Add text to plot
			if (add.text) {
				panel.text(
					x	 = text.info$x,
					y	 = text.info$y,
					labels   = text.info$labels,
					col      = text.info$col,
					cex      = text.info$cex,
					fontface = text.info$fontface
					);
				}
			},
		type = type,
		cex = cex,
		pch = pch,
		col = col,
		lwd = lwd,
		lty = lty,
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
				fontface = if ('Nature' == style) { 'plain' } else {'bold'},
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
					cex = xaxis.cex,
					rot = xaxis.rot,
					limits = xlimits,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
					col = xaxis.col,
					at = xat,
					labels = xaxis.lab,
					log = xaxis.log,
					relation = x.relation,
					alternating = FALSE,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					limits = ylimits,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
					col = yaxis.col,
					at = yat,
					labels = yaxis.lab,
					log = yaxis.log,
					relation = y.relation,
					alternating = FALSE,
					tck = yaxis.tck
					)
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
			),
		layout = layout,
		as.table = as.table,
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd,
				col = if ('Nature' == style) {'transparent'} else { 'black' }
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 1 },
				main.key.padding = 0.1,
				key.top = key.top,
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
				key.left = key.left.padding,
				key.ylab.padding = 0.5,
				ylab = 1,
				ylab.axis.padding = ylab.axis.padding,
				axis.left = 1,
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = 1,
				between = 1,
				axis.right = 1,
				axis.key.padding = axis.key.padding,
				key.right = 1,
				right.padding = right.padding
				),
			strip.background = list(
				col = strip.col
				)
			),
		par.strip.text = list(
			cex = strip.cex
			),
		key = key,
		legend = legend
		);

	if (inside.legend.auto) {
		extra.parameters <- list('x' = trellis.object$panel.args[[1]]$x, 'y' = trellis.object$panel.args[[1]]$y,
			'ylimits' = trellis.object$y.limits, 'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.manhattanplot', filename, trellis.object, height, width, extra.parameters);
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
