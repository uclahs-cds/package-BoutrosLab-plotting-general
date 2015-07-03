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

### FUNCTION TO CREATE HISTOGRAMS ##################################################################
create.histogram <- function(x, data, filename = NULL, xlab.label = NULL, ylab.label = NULL, col = "white", main = NULL, breaks = NULL, xaxis.lab = TRUE, xaxis.cex = 1, xlimits = NULL, xat = TRUE, yaxis.lab = TRUE, yaxis.cex = 1, ylimits = NULL, yat = TRUE, xlab.cex = 2, ylab.cex = 2, main.cex = 2, xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', ylab.col = 'black', xlab.col = 'black', xaxis.tck = 1, yaxis.tck = 1, type = 'percent', x.spacing = 0, y.spacing = 0, top.padding = 0.1, bottom.padding = 0.7, right.padding = 0.1, left.padding = 0.5, ylab.axis.padding = 0, x.relation = "same", y.relation = "same", layout = NULL, strip.col = "white", strip.cex = 1, lwd = 2, lty = 1, abline.h = NULL, abline.v = NULL, abline.col = "black", abline.lwd = 1, abline.lty = 1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', yaxis.fontface = 'bold', style = 'BoutrosLab') {
	
	# 'data' parameter shoud only be set if x if a formula
	# otherwise a warning will be thrown (even if data = NULL), although the plot will still be created 
	# temporarily turn off warnings to avoid confusion
	options(warn = -1);

	trellis.object <- lattice::histogram(
		x = x,
		data = data,
		panel = function(...) {
			# turn warnings back on
			options(warn = 0);
			
			panel.histogram(...);
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
			if (!is.null(abline.h)) {
				panel.abline(
					h = abline.h,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
					);
				}
			if (!is.null(abline.v)) {
				panel.abline(
					v = abline.v,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
					);
				}
			},
		type = type,
		col = col,
		lwd = lwd,
		lty = lty,
		breaks = breaks,
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
					col = xaxis.col,
					rot = xaxis.rot,
					tck = xaxis.tck,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
					limits = xlimits,
					at = xat,
					relation = x.relation,
					alternating = FALSE					
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					rot = yaxis.rot,
					tck = yaxis.tck,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
					limits = ylimits,
					at = yat,
					relation = y.relation,
					alternating = FALSE					
					)
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
			),			
		par.settings = list(
			axis.line = list(
				lwd = 2,
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
				ylab.axis.padding = 1,
				axis.left = 1,
				axis.right = 1,
				axis.key.padding = 0.1,
				key.right = 0.1,
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
