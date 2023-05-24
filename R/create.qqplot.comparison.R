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

### FUNCTION TO CREATE QQPLOT COMPARISON ###########################################################
create.qqplot.comparison <- function(
	x, data = NULL, filename = NULL, groups = NULL, main = NULL, main.just = 'center', main.x = 0.5,
	main.y = 0.5, main.cex = 3, aspect = 'fill', prepanel = NULL, xlab.label = NULL, ylab.label = NULL,
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlimits = NULL, ylimits = NULL,
	xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA,	xaxis.cex = 1.5, yaxis.cex = 1.5,
	xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.log = FALSE, yaxis.log = FALSE, xaxis.rot = 0,
	yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1,
	xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center',
	xlab.top.x = 0.5, xlab.top.y = 0, add.grid = FALSE, xgrid.at = xat, ygrid.at = yat, type = 'p', cex = 0.75,
	pch = 19, col = 'black', lwd = 1, lty = 1, axes.lwd = 2.25, key = list(text = list(lab = c(''))),
	legend = NULL, add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL,
	xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1,
	top.padding = 3, bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.1,
	height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	use.legacy.settings = FALSE, inside.legend.auto = FALSE
	) {

	### needed to copy in case using variable to define rectangles dimensions
        rectangle.info <- list(
        	xright = xright.rectangle,
                xleft = xleft.rectangle,
                ytop = ytop.rectangle,
                ybottom = ybottom.rectangle
                );

	if (!is.null(yat) && length(yat) == 1) {
        	if (yat == 'auto') {
                	out <- auto.axis(unlist(x[[1]]));
                	x[[1]] <- out$x;
                	yat <- out$at;
                	yaxis.lab <- out$axis.lab;
        		}

        	else if (yat == 'auto.linear') {
                	out <- auto.axis(unlist(x[[1]]), log.scaled = FALSE);
                	x[[1]] <- out$x;
                	yat <- out$at;
                	yaxis.lab <- out$axis.lab;
        		}

        	else if (yat == 'auto.log') {
                	out <- auto.axis(unlist(x[[1]]), log.scaled = TRUE);
                	x[[1]] <- out$x;
                	yat <- out$at;
                	yaxis.lab <- out$axis.lab;
        		}
		}

	if (!is.null(xat) && length(xat) == 1) {
        	if (xat == 'auto') {
                	out <- auto.axis(unlist(x[[2]]));
                	x[[2]] <- out$x;
                	xat <- out$at;
                	xaxis.lab <- out$axis.lab;
        		}
        	else if (xat == 'auto.linear') {
                	out <- auto.axis(unlist(x[[2]]), log.scaled = FALSE);
                	x[[2]] <- out$x;
                	xat <- out$at;
                	xaxis.lab <- out$axis.lab;
        		}
        	else if (xat == 'auto.log') {
                	out <- auto.axis(unlist(x[[2]]), log.scaled = TRUE);
                	x[[2]] <- out$x;
                	xat <- out$at;
                	xaxis.lab <- out$axis.lab;
        		}
		}

	# add preloaded defaults
        if (preload.default == 'paper') {
                }
        else if (preload.default == 'web') {
                }

	# x should be a formula or a list of data whose length is 2
	if (as.character(class(x) == 'list')) {

		# create an object to store the data, since the function qq can only handle formula method
		data.to.plot <- data.frame(
			x = c(x[[1]], x[[2]]),
			y = c(rep(1, length(x[[1]])), rep(2, length(x[[2]])))
			);

		formula.to.plot <- y ~ x;
		z <- data.to.plot$x;

		# set x-axis and y-axis label defaults
		# if the label is NULL, then we leave it as blank;
		# if the label is NA, then we use a specific default label.
		if (!is.null(xlab.label) & !is.expression(xlab.label)) {
			if (is.na(xlab.label)) {
				xlab.label <- 'sample one';
				}
			}
		if (!is.null(ylab.label) & !is.expression(ylab.label)) {
			if (is.na(ylab.label)) {
				ylab.label <- 'sample two';
				}
			}
		}

	else {
		formula.to.plot <- x;
		data.to.plot <- data;

		# parse the formula to get x-axis and y-axis labels defaults
		parseform <- latticeParseFormula(as.formula(formula.to.plot), data = data.to.plot);
		y <- parseform$left;
		z <- parseform$right;
		y <- as.factorOrShingle(y);

		# if x-axis labels is NA, set the default
		if (!is.null(xlab.label) & !is.expression(xlab.label)) {
			if (is.na(xlab.label)) {
				# get the name of the first sample from the formula
				if (is.factor(y)) {
					xlab.label <- unique(levels(y))[1];
					}
				else {
					xlab.label <- paste(parseform$left.name, ':', as.character(unique(levels(y)[[1]])));
					}
				}
			}

		# if y-axis labels is NA, set the default
		if (!is.null(ylab.label) & !is.expression(ylab.label)) {
			if (is.na(ylab.label)) {
				# get the name of the second sample from the formula
				if (is.factor(y)) {
					ylab.label <- unique(levels(y))[2];
					}
				else { ylab.label <- paste(parseform$left.name, ':', as.character(unique(levels(y)[[2]]))); }
				}
			}
		}

	# set main label defaults
	# if the label is NULL, then we leave it as blank;
	# if the label is NA, then we use a specific default label.
	if (!is.null(main) & !is.expression(main)) {
		if (is.na(main)) {
			main <- 'Q-Q plot';
			}
		}

	# create the object to store all the data
	trellis.object <- lattice::qq(
		x = formula.to.plot,
		data = data.to.plot,
		panel = function(type.local = type, groups.local = groups, subscripts, identifier = 'qq', ...) {

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

			# if grid-lines are requested, over-ride default behaviour
			if ('g' %in% type || add.grid == TRUE) {
				panel.abline(
					v = BoutrosLab.plotting.general::generate.at.final(
						at.input = xgrid.at,
						limits = xlimits,
						data.vector = z
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = z
						),
					col = trellis.par.get('reference.line')$col
					);
				}

			panel.qq(..., groups = groups.local, subscripts = subscripts, identifier = 'qq');

			},
		aspect = aspect,
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
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					limits = xlimits,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
					at = xat,
					labels = xaxis.lab,
					log = xaxis.log,
					tck = xaxis.tck,
					alternating = FALSE
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					col = yaxis.col,
					limits = ylimits,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
					at = yat,
					labels = yaxis.lab,
					log = yaxis.log,
					tck = yaxis.tck,
					alternating = FALSE
					)
				)
			),
		key = key,
		legend = legend,
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
				ylab.axis.padding = 1,
				axis.left = 1,
				axis.right = 1,
				axis.key.padding = 0.1,
				key.right = 0.1,
				right.padding = right.padding
				)
			)
		);
	if (inside.legend.auto) {
		extra.parameters <- list('x' = trellis.object$panel.args[[1]]$x, 'y' = trellis.object$panel.args[[1]]$y,
			'ylimits' = trellis.object$y.limits, 'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.qqplot.comparison', filename, trellis.object, height, width, extra.parameters);
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
