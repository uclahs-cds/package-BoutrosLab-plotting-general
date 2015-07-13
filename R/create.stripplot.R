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
create.stripplot <- function(formula, data, filename = NULL, groups = NULL, jitter.data = FALSE, jitter.factor = 1, jitter.amount = NULL, main = NULL, xlab.label = tail(sub('~','',formula[-2]),1), ylab.label = tail(sub('~','',formula[-3]),1), xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.fontface = 'bold', yaxis.fontface = 'bold', lwd = 1, pch = 19, col = "black", fill = 'transparent', colour.alpha = 1, cex = 0.75, xaxis.rot = 0, yaxis.rot = 0, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.cex = 2, yaxis.cex = 2, main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', ylab.col = 'black', xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 0, yaxis.tck = 1, top.padding = 0.1, bottom.padding = 0.7, right.padding = 0.3, left.padding = 0.5, ylab.axis.padding = 1, layout = NULL, as.table = TRUE, x.spacing = 0, y.spacing = 0, add.median = FALSE, median.values = NULL, strip.col = "white", strip.cex = 1, strip.fontface = 'bold', width = 7, height = 7, size.units = 'in', resolution = 1600, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, style = 'BoutrosLab') {

	groups.new <- eval(substitute(groups), data, parent.frame());

	trellis.object <- lattice::stripplot(
		formula,
		data,
		panel = function(groups.local = groups.new, subscripts, ...) {
			
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
				warning("median.values must be specified to median to be added.");
				}
			if (add.median && !is.null(median.values)) {
				meds <- median.values;
				xlocs <- seq_along(meds);
				panel.segments(
					xlocs - 1/4, meds, xlocs + 1/4, meds,
					lwd = 2, 
					col = "red"
					);
				}
			},
		type = "p",
		cex = cex,
		pch = pch,
		col = col,
		fill = fill,
		alpha = colour.alpha,
		main = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = if ('Nature' == style){'plain'} else('bold')
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = if ('Nature' == style){'plain'} else('bold')
				)
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					limits = xlimits,
					at = xat,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
					alternating = FALSE,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					limits = ylimits,
					at = yat,
					tck = yaxis.tck,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
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
				col = if ('Nature' == style){'transparent'} else('black')
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3} else { 1 },
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
