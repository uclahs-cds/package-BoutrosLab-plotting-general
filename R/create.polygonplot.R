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

### FUNCTION TO CREATE POLYGONPLOT ################################################################
create.polygonplot <- function(formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5, groups = NULL, max, min, col = 'white', border.col = 'black', xy.col = 'black', strip.col = 'white', strip.cex = 1, type = 'p', cex = 0.75, pch = 19, lwd = 1, lty = 1, axes.lwd = 1, xlab.label = tail(sub('~','',formula[-2]),1), ylab.label = tail(sub('~','',formula[-3]),1), main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', ylab.col = 'black',xlab.top.label = NULL,xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = "center", xlab.top.x = 0.5, xlab.top.y = 0, xaxis.rot = 0, xaxis.cex = 2, yaxis.rot = 0, yaxis.cex = 2, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.log = FALSE, yaxis.log = FALSE, xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.col = 'black', yaxis.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, x.spacing = 0, y.spacing = 0, top.padding = 0.5, bottom.padding = 2, right.padding = 1, left.padding = 2, ylab.axis.padding = 0, x.relation = "same", y.relation = "same", xaxis.tck = 1, yaxis.tck = 1, layout = NULL, as.table = FALSE, add.xy.border = FALSE, add.median = FALSE, median.lty = 3, use.loess.border = FALSE, use.loess.median = FALSE, median = NULL, median.col = "black", extra.points = NULL, extra.points.pch = 21, extra.points.type = 'p', extra.points.col = 'black', extra.points.fill = 'white', extra.points.cex = 1, xgrid.at = xat, ygrid.at = yat, grid.lty = 1, grid.col = "grey", grid.lwd = 0.3, add.xyline = FALSE, xyline.col = "black", xyline.lwd = 1, xyline.lty = 1, abline.h = NULL, abline.v = NULL, abline.col = "black", abline.lwd = 1, abline.lty = 1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL, style = 'BoutrosLab') {

	groups.new <- eval(substitute(groups), data, parent.frame());
	if (length(xat) ==1 && xat == TRUE && length(xlimits) == 0) {
		
		

		if (!is.null(data)) {
			minimum <- 0;
			maximum <- length(data[[1]]);
		        difference <- maximum - minimum;
                        lognumber <- floor(log(difference,10));
                        
                        # depending on difference, the labels will be multiples of 5,10 or 20
                        if (difference < (10**lognumber*4)) { factor <- (10**lognumber)/2; }
                        else if (difference < (10**lognumber*7)) { factor <- (10**lognumber); }
                        else { factor <- (10**lognumber)*2; }
                        
                        addition <- factor/2;
                        
                        # depending on minimum create a sequence of at locations with padding
                        
                        at <- seq(0,factor*round(maximum/factor) + addition,factor);
                       

			xlimits <- c(minimum,maximum);
			xat <- at;
			}
		}

	if (length(yat) == 1 && yat == TRUE && length(ylimits) == 0) {
		
		
		if(!is.null(data)) {
			minimum <- min(min)
			maximum <- max(max);
		
			# if minimum is greater than 0 make sure to display 0
			minimum <- min(minimum,0);
			difference <- maximum - minimum;
			lognumber <- floor(log(difference,10));

			# depending on difference, the labels will be multiples of 5,10 or 20
			if (difference < (10**lognumber*4)) { factor <- (10**lognumber)/2; }
			else if (difference < (10**lognumber*7)) { factor <- (10**lognumber); }
			else { factor <- (10**lognumber)*2; }

			addition <- factor/2;

			# depending on minimum create a sequence of at locations with padding 
			if (minimum == 0) { 
				at <- seq(0,factor*round(maximum/factor) + addition,factor); 
				}
			else { 
				at <- seq(factor*round(minimum/factor),factor*round(maximum/factor) + addition,factor);
				# only add padding to minimum if it is not 0
				minimum <- minimum - addition;
				}
			# add padding to max
			maximum <- maximum + addition;
			ylimits <- c(minimum,maximum);
			yat <- at;
			}
		}

	trellis.object <- lattice::xyplot(
		formula,
		data,
		max = max,
		min = min,
		median = median,
		panel = function(x, y, col = col, border = border.col, groups = groups.new, subscripts, ...) {
			if (is.null(groups.new)) {

				# draw polygon
				panel.polygon(
					x = c(x, rev(x)), 
					y = if (use.loess.border) { c(predict(loess(max[subscripts] ~ x)), rev(predict(loess(min[subscripts] ~ x)))) } else { c(max[subscripts], rev(min[subscripts])) } ,  
					col = col, 
					...
					);

				# draw xy points along border of polygon
				if (add.xy.border) {
					panel.xyplot( 
						x = c(x, rev(x)), 
						y = c(max[subscripts], rev(min[subscripts])),
						type = 'p',
						col = xy.col
						);
					}
				
				# draw median line
				if (add.median & !is.null(median)) {
					panel.xyplot(
						x = x, 
						y = if (use.loess.median) { predict(loess(median[subscripts] ~ x)) } else { median[subscripts] }, 
						type = 'l',
						lwd = 1.5,
						col = median.col,
						lty = median.lty
						);
					}

				# draw extra points
				if (!is.null(extra.points)) {
					panel.xyplot(
						x = extra.points$x,
						y = extra.points$y,
						groups = groups,
						subscripts = subscripts,
						type = extra.points.type,
						pch = extra.points.pch,
						col = extra.points.col,
						cex = extra.points.cex,
						fill = extra.points.fill
						);
					}

				# add background grid
				if (!is.null(xgrid.at) || !is.null(ygrid.at)) {
					panel.abline(
						v = xgrid.at,
						h = ygrid.at,
						lty = grid.lty,
						col = grid.col,
						lwd = grid.lwd,
						alpha = 0.5
						)
					}

				# if requested, add y=x line
				if (add.xyline) {
					panel.abline(
						a = 0, 
						b = 1, 
						lwd = xyline.lwd,
						lty = xyline.lty,
						col = xyline.col
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
				
				# if requested, add user-defined vertical line
				if (!is.null(abline.v)) {
					panel.abline(
						v   = abline.v,
						lty = abline.lty,
						lwd = abline.lwd,
						col = abline.col
						);
					}
						
				}
				
			else {
				# can't use ternary operator because need to return vectors
				border.col <- if (length(border.col) == 1) {
					rep(border.col, length(subscripts));
					} 
				else {
					as.character(factor(x = groups, labels = border.col));
					}
				median.col <- if (length(median.col) == 1) {
					rep(median.col, length(subscripts));
					}	
				else {
					as.character(factor(x = groups, labels = median.col));
					}
				median.lty <- if (length(median.lty) == 1) {
					rep(median.lty, length(subscripts));
					}
				else {
					as.character(factor(x = groups, labels = median.lty));
					}
				extra.points.col <- if (length(extra.points.col) == 1) {
					rep(extra.points.col, length(subscripts));
					} 
				else {
					as.character(factor(x = groups, labels = extra.points.col));
					}
				# need to cast twice to retrieve integer pch values
				extra.points.pch <- if (length(extra.points.pch) == 1) {
					rep(extra.points.pch, length(subscripts));
					} 
				else {
					as.integer(as.character(factor(x = groups, labels = extra.points.pch)));
					}

				panel.superpose(
					x, 
					y, 
					groups = groups.new, 
					subscripts,
					panel.groups = function(x, y, max, min, groups = groups.new, subscripts, type, ..., font, fontface) {

						# draw polygon
						panel.polygon(
							x = c(x, rev(x)), 
							y = if (use.loess.border) {c(predict(loess(max[subscripts] ~ x)), rev(predict(loess(min[subscripts] ~ x)))) } else { c(max[subscripts], rev(min[subscripts])) } , 
							type, 
							...
							);

						# draw polygon borders
						panel.xyplot(
							x = c(x, rev(x), x[1]),
							y = c(max[subscripts], rev(min[subscripts]), max[subscripts][1]),
							type = 'l',
							col = border.col[subscripts],
							lwd = lwd
							);

						# draw median line
						if (add.median & !is.null(median)) {
							panel.xyplot( 
								x = x,
								y = if (use.loess.median) { predict(loess(median[subscripts] ~ x)) } else { median[subscripts] }, 
								type = 'l',
								col = median.col[subscripts],
								lty = median.lty[subscripts]
								);
							}

						# add extra points, assuming same grouping as original data
						if (!is.null(extra.points)) {
							panel.xyplot(
							x = x,
							y = extra.points$y[subscripts],
							groups = groups,
							subscripts = subscripts,
							type = extra.points.type,
							pch = extra.points.pch[subscripts],
							col = extra.points.col[subscripts],
							cex = extra.points.cex,
							fill = extra.points.fill
							);
						}

						# add background grid
						if (!is.null(xgrid.at) || !is.null(ygrid.at)) {
							panel.abline(
								v = xgrid.at,
								h = ygrid.at,
								lty = grid.lty,
								col = grid.col,
								lwd = grid.lwd,
								alpha = 0.5
								);
							}

						# if requested, add y=x line
						if (add.xyline) {
							panel.abline(
								a = 0, 
								b = 1, 
								lwd = xyline.lwd,
								lty = xyline.lty,
								col = xyline.col
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
						
						# if requested, add user-defined vertical line
						if (!is.null(abline.v)) {
							panel.abline(
								v   = abline.v,
								lty = abline.lty,
								lwd = abline.lwd,
								col = abline.col
								);
							}

						},
					alpha = 0.5, 
					col = col,
					border = "white",
					...
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
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = main.cex,
				just = main.just,
				x = main.x,
				y = main.y
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = if ('Nature' == style){'plain'} else('bold')
				)
			),
                xlab.top = BoutrosLab.plotting.general::get.defaults(
                        property = 'fontfamily',
                        add.to.list = list(
                                label = xlab.top.label,
                                cex = xlab.top.cex,
                                col = xlab.top.col,
                                fontface = if('Nature' == style){'plain'}else{'bold'},
                                just = xlab.top.just,
                                x = xlab.top.x,
				y = xlab.top.y
                                )
                        ),

		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = if ('Nature' == style){'plain'} else('bold')
				)
			),			
		between = list(
			x = x.spacing, 
			y = y.spacing
			),	
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily', 
				add.to.list = list(
					labels = xaxis.lab,
					log = xaxis.log,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
					rot = xaxis.rot,
					limits = xlimits,
					cex = xaxis.cex,
					col = xaxis.col,
					at = xat,
					relation = x.relation,
					alternating = FALSE,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily', 
				add.to.list = list(
					labels = yaxis.lab,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
					limits = ylimits,
					cex = yaxis.cex,
					col = yaxis.col,
					rot = yaxis.rot,
					tck = yaxis.tck,
					at = yat,
					log = yaxis.log,
					relation = y.relation
					)
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd,
				col = if ('Nature' == style){'transparent'} else('black')
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 3 },
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
				axis.key.padding = 1,
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
		layout = layout,
		as.table = as.table,
		pretty = TRUE,
		key = key,
		legend = legend
		);

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Re-add bottom and left axes
		trellis.object$axis = function(side, line.col = "black", ...) {
			# Only draw axes on the left and bottom
			if(side %in% c("bottom","left")) {
				axis.default(side = side, line.col = "black", ...);
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

		warning("Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend")
		} 

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
