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
create.manhattanplot <- function(formula, data, filename = NULL, main = NULL, xlab.label = NULL, ylab.label = NULL, main.cex = 2, xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA, xaxis.log = FALSE, yaxis.log = FALSE, xaxis.cex = 1, yaxis.cex = 1, xaxis.rot = 0, yaxis.rot = 0, xaxis.fontface = 'plain', yaxis.fontface = 'plain', xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 0, yaxis.tck = c(1,1), type = 'p', cex = 2, pch = '.', col = 'black', strip.col = "white", strip.cex = 1, lwd = 1, lty = 1, alpha = 1, axis.lwd = 1, key = list(text = list(lab = c(''))), legend = NULL, x.spacing = 0, y.spacing = 0, top.padding = 0, bottom.padding = 0, right.padding = 0, left.padding = 0, key.top = 0, ylab.axis.padding = 1, axis.key.padding = 1, x.relation = "same", y.relation = "same", layout = NULL, as.table = FALSE, axes.lty = 'dashed', abline.h = NULL, abline.col = "black", abline.lwd = 1, abline.lty = 1, add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, add.points = FALSE, points.x = NULL, points.y = NULL, points.pch = 19, points.col = 'black', points.cex = 1, add.text = FALSE, text.labels = NULL, text.x = NULL, text.y = NULL, text.col = 'black', text.cex = 1, text.fontface = 'bold', key.left.padding = 0, height = 6, width = 10, size.units = 'in', resolution = 1600, enable.warnings = FALSE, horizontal = FALSE, description = NULL, ...) {

	trellis.object <- lattice::xyplot(
		formula,
		data,
		panel = function(subscripts, type.local = type, abline.local = abline, ...) {
				
			# if requested, add user-defined horizontal line
			if (!is.null(abline.h)) {
				panel.abline(
					h   = abline.h,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
					);
				}
						
			# if requested, add user-defined rectangle (ie, confidence intervals)
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
						
			# create the main plot
			panel.xyplot(
				subscripts = subscripts,
				type = setdiff(type.local, 'g'),
				alpha = alpha,
				horizontal = horizontal,
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
			add.to.list = list(
				label = main,
				fontface = 'bold',
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = 'bold'
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = 'bold'
				)
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily', 
				add.to.list = list(
					cex = xaxis.cex,
					rot = xaxis.rot,
					limits = xlimits,
					fontface = xaxis.fontface,
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
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					limits = ylimits,
					fontface = yaxis.fontface,
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
				lwd = axis.lwd
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3} else { 1 },
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
