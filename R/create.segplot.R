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

### FUNCTION TO CREATE SEGPLOTS ###################################################################
create.segplot <- function(formula, data, filename = NULL, main = NULL, lwd = 1, xlab.label = NULL, ylab.label = NULL, main.cex = 3, xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xaxis.fontface = 'plain', yaxis.fontface = 'plain', xaxis.rot = 0, yaxis.rot = 0, xaxis.cex = 1.5, yaxis.cex = 2, xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.col = 'black', yaxis.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, abline.h = NULL, abline.v = NULL, abline.lty = 1, abline.lwd = 1, abline.col = 'black', segments.col = 'black', segments.lwd = 1, x.spacing = 0, y.spacing = 0, top.padding = 0.5, bottom.padding = 2, right.padding = 1, left.padding = 2, ylab.axis.padding = 0, x.relation = "same", y.relation = "same", xaxis.tck = 1, yaxis.tck = 1, level = NULL, col.regions =NULL, centers = NULL, horizontal = TRUE, draw.bands =  FALSE, pch = 16, symbol.col = 'black', symbol.cex = 1, layout = NULL, as.table = FALSE, height = 6, width = 6, size.units = 'in', resolution = 1000, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1) {

	trellis.object <- lattice::levelplot(
		x = formula,
		data,
		prepanel = prepanel.segplot,
		panel = function( abline.local = abline, ...) {
			
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

			# if requested, add user-defined vertical line
			if (!is.null(abline.v)) {
				panel.abline(
					v   = abline.v,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
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
			panel.segplot(
				col = segments.col,
				fill = segments.col,
				lwd = segments.lwd,
				pch = pch,
				col.regions = col.regions,
				level = level,
				centers = centers,
				horizontal = horizontal,
				draw.bands = draw.bands,
				...,
				)
			},
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
		between = list(
			x = x.spacing, 
			y = y.spacing
			),	
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = xaxis.lab,
					rot = xaxis.rot,
					limits = xlimits,
					cex = xaxis.cex,
					col = xaxis.col,
					fontface = xaxis.fontface,
					at = xat,
					relation = x.relation,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					fontface = yaxis.fontface,
					rot = yaxis.rot,
					tck = yaxis.tck,
					limits = ylimits,
					at = yat,
					relation = y.relation,
					alternating = FALSE					
					)
				),
			alternating = 1
			),
		par.settings = list(
			axis.line = list(
				lwd = lwd
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
			plot.symbol = list(
				col = symbol.col,
				cex = symbol.cex
				)
			),
		colorkey = FALSE,
		layout = layout,
		as.table = as.table,	
		pretty = TRUE,
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
