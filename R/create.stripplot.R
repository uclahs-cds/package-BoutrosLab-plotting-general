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
create.stripplot <- function(formula, data, filename = NULL, groups = NULL, jitter.data = FALSE, jitter.factor = 1, jitter.amount = NULL, main = NULL, xlab.label = NULL, ylab.label = NULL, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.fontface = 'bold', yaxis.fontface = 'bold', lwd = 1, pch = 19, col = "black", fill = 'transparent', colour.alpha = 1, cex = 0.75, xaxis.rot = 0, yaxis.rot = 0, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.cex = 2, yaxis.cex = 2, main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', ylab.col = 'black', xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 0, yaxis.tck = 1, top.padding = 0.1, bottom.padding = 0.7, right.padding = 0.3, left.padding = 0.5, ylab.axis.padding = 1, layout = NULL, as.table = TRUE, x.spacing = 0, y.spacing = 0, strip.col = "white", strip.cex = 1, strip.fontface = 'bold', width = 7, height = 7, size.units = 'in', resolution = 1000, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1) {

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
				fontface = "bold",
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = "bold"
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = "fontfamily", 
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = "bold"
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
					fontface = xaxis.fontface,
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
					fontface = yaxis.fontface,
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
				lwd = lwd
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
