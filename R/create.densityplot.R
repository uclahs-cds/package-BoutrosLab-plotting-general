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

### FUNCTION TO CREATE DENSITYPLOTS ################################################################
create.densityplot <- function(x, filename = NULL, xlab.label = NULL, main = NULL, type = "l", lty = 'solid', cex = 0.75, pch = 19, col = 'black', lwd = 2, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xgrid.at = xat, ygrid.at = yat, xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 1, yaxis.cex = 1, xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1, main.cex = 2, xlab.cex = 2, ylab.cex = 2, ylab.label = 'Density', xlab.col = 'black', ylab.col = 'black', key = list(text = list(lab = c(''))), legend = NULL, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.1, add.axes = FALSE, abline.h = NULL, abline.v = NULL, abline.type = NULL, abline.lwd = NULL, abline.col = 'black', height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', yaxis.fontface = 'bold', style = 'BoutrosLab') {

	# create an object to store all the data
	data.to.plot <- data.frame(
		x = rep(0, 512 * length(x)),
		y = rep(0, 512 * length(x)),
		groups = rep(NA, 512 * length(x))
		);
	
	for (i in 1:length(x)) {
		this.density <- density(x[[i]], na.rm = TRUE);
		start.point <- 1 + (i - 1) * 512;
		end.point <- start.point + 512 - 1;
		data.to.plot$x[start.point:end.point] <- this.density$x;
		data.to.plot$y[start.point:end.point] <- this.density$y;
		data.to.plot$groups[start.point:end.point] <- rep(names(x)[i], 512);
		}
	
	# avoid groups being plotted in alphabetical factor order.  
	# this seems to cause disjoint with other parameters like col
	data.to.plot$groups <- factor(data.to.plot$groups,levels = unique(data.to.plot$groups));

	if (length(yat) == 1 && yat == TRUE && length(ylimits) == 0) {
		
		maximum <- max(data.to.plot$y);
		
		# if minimum is greater than 0 make sure to display 0
		lognumber <- floor(log(maximum, 10));

		# depending on difference, the labels will be multiples of 5,10 or 20
		if (maximum < (10**lognumber*4)) { factor <- (10**lognumber)/2; }
		else if (maximum < (10**lognumber*7)) { factor <- (10**lognumber); }
		else { factor <- (10**lognumber)*2; }

		addition <- factor/2;

		# depending on minimum create a sequence of at locations with padding 
		at <- seq(0,factor*round(maximum/factor) + addition,factor);
		maximum <- maximum + addition;
		ylimits <- c(0, maximum);
		yat <- at;
		}

	if (length(xat) == 1 && xat == TRUE && length(xlimits) == 0) {
		minimum <- min(data.to.plot$x);
		maximum <- max(data.to.plot$x);
		
		# if minimum is greater than 0 make sure to display 0
		minimum <- min(minimum, 0);
		difference <- maximum - minimum;
		lognumber <- floor(log(difference, 10));

		# depending on difference, the labels will be multiples of 5,10 or 20
		if (difference < (10**lognumber*4)) { factor <- (10**lognumber)/2; }
		else if (difference < (10**lognumber*7)){ factor <- (10**lognumber); }
		else { factor <- (10**lognumber)*2; }

		addition <- factor/2;

		# depending on minimum create a sequence of at locations with padding 
		if (minimum == 0) { at <- seq(0,factor*round(maximum/factor) + addition,factor); }
		else { 
			at <- seq(factor*round(minimum/factor),factor*round(maximum/factor) + addition,factor);
			# only add padding to minium if it is not 0
			minimum <- minimum - addition;
			}

		# add padding to max
		maximum <- maximum + addition;
		xlimits <- c(minimum, maximum);
		xat <- at;
		}

	# create the plot
	trellis.object <- lattice::xyplot(
		y ~ x,
		data.to.plot,
		panel = function(groups.local = data.to.plot$groups, subscripts, type.local = type, ...) {

			panel.abline(h = abline.h, lty = abline.type, lwd = abline.lwd, col = abline.col);
			panel.abline(v = abline.v, lty = abline.type, lwd = abline.lwd, col = abline.col);

			# if requested, add x=0, y=0 lines
			if (add.axes) {
				panel.abline(
					h = 0,
					v = 0,
					col.line = "black",
					lty = "dashed",
					lwd = 1.5
					);
				}
				
			# add rectangle if requested
			if (add.rectangle) {
				panel.rect(
					xleft = xleft.rectangle,
					ybottom = ybottom.rectangle,
					xright = xright.rectangle,
					ytop = ytop.rectangle,
					col = col.rectangle,
					alpha = alpha.rectangle,
					border = NA
					);
				}

			# if grid-lines are requested, over-ride default behaviour
			if ("g" %in% type) {
				panel.abline(
					v = BoutrosLab.plotting.general::generate.at.final(
						at.input = xgrid.at,
						limits = xlimits,
						data.vector = data.to.plot$x
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = data.to.plot$y
						),
					col = trellis.par.get("reference.line")$col
					);
				panel.xyplot(
					groups = groups.local,
					grid = FALSE, 
					subscripts = subscripts,
					type = setdiff(type.local, 'g'),
					...
					);
				}

			# create the main plot
			panel.xyplot(
				groups = groups.local,
				subscripts = subscripts,
				type = setdiff(type.local, 'g'),
				...
				);
			},
		type = type,
		lwd = lwd,
		lty = lty,
		col = col,
		main = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style){'plain'} else("bold"),
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = xlab.label,
				fontface = if ('Nature' == style){'plain'} else("bold"),
				cex = xlab.cex,
				col = xlab.col
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = ylab.label,
				fontface = if ('Nature' == style){'plain'} else("bold"),
				cex = ylab.cex,
				col = ylab.col
				)
			),
		scales = list(
			x = get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
					limits = xlimits,
					axs = "r",
					at = xat,
					tck = xaxis.tck,
					labels = xaxis.lab
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					col = yaxis.col,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
					limits = ylimits,
					at = yat,
					tck = xaxis.tck,
					labels = yaxis.lab
					)
				)
			),
		key = key,
		legend = legend,
		par.settings = list(
			axis.line = list(
				lwd = 2.25,
				col = if ('Nature' == style){'transparent'} else('black')
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
