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

### FUNCTION TO CREATE QQPLOT FIT #################################################################
create.qqplot.fit <- function(
	x, data = NA, filename = NULL, groups = NULL, confidence.bands = FALSE, conf = 0.95,
	confidence.method = 'both', reference.line.method = 'quartiles', distribution = qnorm, aspect = 'fill',
	prepanel = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3,
	xlab.label = NULL, ylab.label = NULL, xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black',
	xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5,
	xlab.top.y = 0, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA,
	xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.col = 'black', yaxis.col = 'black', xaxis.fontface = 'bold',
	yaxis.fontface = 'bold', xaxis.log = FALSE, yaxis.log = FALSE, xaxis.rot = 0, yaxis.rot = 0, xaxis.tck = 1,
	yaxis.tck = 1, add.grid = FALSE, xgrid.at = xat, ygrid.at = yat, type = 'p', cex = 0.75, pch = 19, col = 'black',
	col.line = 'grey', lwd = 2, lty = 1, axes.lwd = 2.25, key = list(text = list(lab = c(''))), legend = NULL,
	add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL,
	ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, top.padding = 3, bottom.padding = 0.7,
	left.padding = 0.5, right.padding = 0.1, height = 6, width = 6, size.units = 'in', resolution = 1600,
	enable.warnings = FALSE, description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, inside.legend.auto = FALSE
	) {

	### store data on mount
        tryCatch({
                        dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if( !dir.exists(dir.name) ) {
                                dir.create(dir.name);
                                }                        
			funcname <- 'create.qqplot.fit';
                        if(is.null(data)) {
                                print.to.file(dir.name, funcname, x, filename);
                                }
                        else {
                                print.to.file(dir.name, funcname, data, filename);
                                }

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

	# set main, x-axis and y-axis label defaults
	# if the label is NULL, then we leave it as blank;
	# if the label is NA, then we use a specific default label.
	if (!is.null(main) & !is.expression(main)) {
		if (is.na(main)) {
			main <- 'Q-Q plot';
			}
		}

	if (!is.null(xlab.label) & !is.expression(xlab.label)) {
		if (is.na(xlab.label)) {
			xlab.label <- deparse(substitute(distribution));
			}
		}

	if (!is.null(ylab.label) & !is.expression(ylab.label)) {
		if (is.na(ylab.label)) {
			ylab.label <- latticeParseFormula(as.formula(x), data = data)$right.name;
			}
		}

	# create the object to store all the data
	trellis.object <- lattice::qqmath(
		x = x,
		data = data,
		distribution = distribution,
		aspect = aspect,
		prepanel = prepanel.qqmathline,
		panel = function(
			x,
			type.local = type,
			groups.local = groups,
			subscripts,
			distribution.local = distribution,
			col.local = col.line,
			col = col,
			lwd = lwd,
			...
			) {

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
						data.vector = x
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = x
						),
					col = trellis.par.get('reference.line')$col
					);
				}

			# draw the reference line, could be one of the following:
			#	quartile: across 1/4 and 3/4 quantiles
			#	diagonal: abline(0,1),
			#	robust: best fit by linear regression
			if (reference.line.method == 'quartiles') {
				panel.qqmathline(
					x,
					distribution = distribution.local,
					groups = groups.local,
					subscripts = subscripts,
					lwd = lwd,
					col = col.line,
					...
					);
				}

			if (reference.line.method == 'diagonal' & !confidence.bands) {
				panel.abline(0, 1);
				}

			if (reference.line.method == 'robust' & !confidence.bands) {
				tmp.data <- BoutrosLab.plotting.general::create.qqplot.fit.confidence.interval(
					x = x,
					distribution = distribution,
					conf = conf,
					conf.method = confidence.method,
					reference.line.method = reference.line.method
					);

				a <- tmp.data$a;
				b <- tmp.data$b;
				panel.abline(a, b);
				}

			# if confidence bands are requested
			if (confidence.bands) {

				# for non-grouped data
				if (is.null(groups)) {

					# store the value to create the confidence bands
					tmp.ci <- BoutrosLab.plotting.general::create.qqplot.fit.confidence.interval(
						x = x,
						distribution = distribution,
						conf = conf,
						conf.method = confidence.method,
						reference.line.method = reference.line.method
						);

					if (!reference.line.method == 'quartiles') {
						panel.abline(tmp.ci$a, tmp.ci$b);
						}

					# using the returned value to plot
					if (confidence.method == 'both') {
						panel.polygon(
							x = c(tmp.ci$z[tmp.ci$u], rev(tmp.ci$z[tmp.ci$l])),
							y = c(tmp.ci$upper.sim, rev(tmp.ci$lower.sim)),
							col = '#e6e6e6',
							border = '#e6e6e6',
							alpha = 0.5
							);

						if (confidence.method == 'both') {
							panel.lines(tmp.ci$z, tmp.ci$upper.pw, lty = 1, lwd = lwd, col = '#b5b5b5');
							panel.lines(tmp.ci$z, tmp.ci$lower.pw, lty = 1, lwd = lwd, col = '#b5b5b5');
							panel.lines(tmp.ci$z[tmp.ci$u], tmp.ci$upper.sim, lty = 1, lwd = lwd, col = '#e6e6e6');
							panel.lines(tmp.ci$z[tmp.ci$l], tmp.ci$lower.sim, lty = 1, lwd = lwd, col = '#e6e6e6');
							}
						else {
							if (confidence.method == 'simultaneous') {
								panel.lines(tmp.ci$z[tmp.ci$u], tmp.ci$upper.sim, lty = 1, lwd = lwd, col = '#e6e6e6');
								panel.lines(tmp.ci$z[tmp.ci$l], tmp.ci$lower.sim, lty = 1, lwd = lwd, col = '#e6e6e6');
								}
							if (confidence.method == 'pointwise') {
								panel.lines(tmp.ci$z, tmp.ci$upper.pw, lty = 1, lwd = 2, col = '#b5b5b5');
								panel.lines(tmp.ci$z, tmp.ci$lower.pw, lty = 1, lwd = 2, col = '#b5b5b5');
								}
							}

						panel.polygon(
							x = c(tmp.ci$z, rev(tmp.ci$z)),
							y = c(tmp.ci$upper.pw, rev(tmp.ci$lower.pw)),
							col = '#b5b5b5',
							border = '#b5b5b5',
							alpha = 0.5
							);

						draw.key(
							list(
								text = list(
									lab = c('pointwise', 'simultaneous')
									),
								points = list(
									pch = 15,
									col = c('#b5b5b5', '#e6e6e6')
									)
								),
							draw = TRUE,
							vp = viewport(x = unit(0.82, 'npc'), y = unit(0.06, 'npc'))
							);
						}
					else {
						if (confidence.method == 'simultaneous') {
							panel.polygon(
								x = c(tmp.ci$z[tmp.ci$u], rev(tmp.ci$z[tmp.ci$l])),
								y = c(tmp.ci$upper.sim, rev(tmp.ci$lower.sim)),
								col = '#e6e6e6',
								border = '#e6e6e6',
								alpha = 0.5
								);
							}

						if (confidence.method == 'pointwise') {
							panel.polygon(
								x = c(tmp.ci$z, rev(tmp.ci$z)),
								y = c(tmp.ci$upper.pw, rev(tmp.ci$lower.pw)),
								col = '#b5b5b5',
								border = '#b5b5b5',
								alpha = 0.5
								);
							}
						}
					}

				# for grouped data
				else {
					grouped.data <- split(x, groups);
					groups.names <- sort(unique(groups.local));
					number.groups <- length(groups.names);

					for (k in 1:number.groups) {

						# store the value to create the confidence bands for each group
						tmp.ci <- BoutrosLab.plotting.general::create.qqplot.fit.confidence.interval(
							x = grouped.data[[groups.names[k]]],
							distribution = distribution,
							conf = conf,
							conf.method = confidence.method
							);

						if (!reference.line.method == 'quartiles') {
							panel.abline(tmp.ci$a, tmp.ci$b);
							}

						# using the returned value to plot
						if (confidence.method == 'both') {

							panel.polygon(
								x = c(tmp.ci$z[tmp.ci$u], rev(tmp.ci$z[tmp.ci$l])),
								y = c(tmp.ci$upper.sim, rev(tmp.ci$lower.sim)),
								col = '#e6e6e6',
								border = '#e6e6e6',
								alpha = 0.5
								);

							panel.polygon(
								x = c(tmp.ci$z, rev(tmp.ci$z)),
								y = c(tmp.ci$upper.pw, rev(tmp.ci$lower.pw)),
								col = '#b5b5b5',
								border = '#b5b5b5',
								alpha = 0.5
								);

							# draw the key to indicate the two methods only once
							if (1 == k) {
								draw.key(
									list(
										text = list(
											lab = c('pointwise', 'simultaneous')
											),
										points = list(
											pch = 15,
											col = c('#b5b5b5', '#e6e6e6')
											)
										),
									draw = TRUE,
									vp = viewport(x = unit(0.82, 'npc'), y = unit(0.06, 'npc'))
									);
								}
							}

						else {
							if (confidence.method == 'simultaneous') {
								panel.polygon(
									x = c(tmp.ci$z[tmp.ci$u], rev(tmp.ci$z[tmp.ci$l])),
									y = c(tmp.ci$upper.sim, rev(tmp.ci$lower.sim)),
									col = '#e6e6e6',
									border = '#e6e6e6',
									alpha = 0.5
									);
								}

							if (confidence.method == 'pointwise') {
								panel.polygon(
									x = c(tmp.ci$z, rev(tmp.ci$z)),
									y = c(tmp.ci$upper.pw, rev(tmp.ci$lower.pw)),
									col = '#b5b5b5',
									border = '#b5b5b5',
									alpha = 0.5
									);
								}
							}
						}
					}
				}

			# draw the main plot
			panel.qqmath(
				x,
				distribution = distribution.local,
				groups = groups.local,
				subscripts = subscripts,
				col = col,
				...
				);
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
                extra.parameters <- list('x' = trellis.object$panel.args[[1]]$x,
			'ylimits' = trellis.object$y.limits, 'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.qqplot.fit', filename, trellis.object, height, width, extra.parameters);
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
