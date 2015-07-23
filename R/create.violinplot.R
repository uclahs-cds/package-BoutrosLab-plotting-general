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

### FUNCTION TO CREATE VIOLIN PLOTS ###############################################################
create.violinplot <- function(formula, data, filename = NULL, main = NULL, xlab.label = tail(sub('~','',formula[-2]),1), ylab.label = tail(sub('~','',formula[-3]),1), xaxis.lab = TRUE, yaxis.lab = TRUE, lwd = 1, xaxis.rot = 0, yaxis.rot = 0, ylimits = NULL, yat = TRUE, xaxis.cex = 2, yaxis.cex = 2, main.cex = 3, xlab.cex = 3, ylab.cex = 3, xlab.col = 'black', ylab.col = 'black', xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = c(1,0), yaxis.tck = c(1,1), col = "black", border.lwd = 1, extra.points = NULL, extra.points.pch = 21, extra.points.col = "white", extra.points.border = "black", extra.points.cex = 1, start = NULL, end = NULL, scale = FALSE, plot.horizontal = FALSE, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.3, width = 6, height = 6, resolution = 1600, size.units = 'in', enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, xaxis.fontface = 'bold', yaxis.fontface = 'bold', style = 'BoutrosLab') {

	trellis.object <- lattice::bwplot(
		formula,
		data,
		panel = function(from = start, to = end, varwidth = scale, ...) {
			
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
			if (is.null(from) || is.null(to)) {
				panel.violin(varwidth = varwidth, ...);
				}
			else {		
				panel.violin(from = from, to = to, varwidth = varwidth, ...);
				}
		
			if (!is.null(extra.points)) {	
				for (i in 1:length(extra.points)) {

					if (is.na(extra.points.pch[i])) extra.points.pch[i] <- extra.points.pch[1];
					if (is.na(extra.points.col[i])) extra.points.col[i] <- extra.points.col[1];
					if (is.na(extra.points.cex[i])) extra.points.cex[i] <- extra.points.cex[1];
					if (is.na(extra.points.border[i])) extra.points.border[i] <- extra.points.border[1];

					for (j in 1:length(extra.points[[i]])) {
						if (!is.na(extra.points[[i]][j])) {
							panel.xyplot(
								x = j,
								y = extra.points[[i]][j],
								pch = extra.points.pch[i],
								col = if(extra.points.pch[i] %in% 0:20) { extra.points.col[i]; } else 
									if (extra.points.pch[i] %in% 21:25) { extra.points.border[i]; },
								fill = if(extra.points.pch[i] %in% 0:20) { NA; } else 
									if (extra.points.pch[i] %in% 21:25) { extra.points.col[i]; },
								cex = extra.points.cex[i]
								);
							}
						}
					}
				}
			},
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
			lwd = 1,
			x = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					tck = xaxis.tck,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface)
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					limits = ylimits,
					at = yat,
					rot = yaxis.rot,
					col = yaxis.col,
					tck = yaxis.tck,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface)
					)
				)
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
				ylab.axis.padding = 1,
				axis.left = 1,
				axis.right = 1,
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = 1,
				between = 1,
				axis.right = 1,
				axis.key.padding = 0.1,
				right.padding = right.padding
				),
			box.dot = list(
				pch = 19,
				col = "#000000",
				lty = 1
				),
			box.rectangle = list(
				lwd = 3,
				col = "#000000",
				lty = 1
				),
			box.umbrella =list(
				lwd = 2,
				col = "#000000",
				lty = 1
				),
			plot.symbol = list(
				col = "#000000",
				pch = 19,
				cex = 0.5
				),
			plot.polygon = list(
				col = col,
				lwd = border.lwd
				)
			),
		pch = "|",
		pretty = TRUE,
		horizontal = plot.horizontal,
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
