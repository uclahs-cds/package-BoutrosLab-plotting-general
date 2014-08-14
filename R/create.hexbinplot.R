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

### FUNCTION TO CREATE HEXBINPLOTS #################################################################
create.hexbinplot <- function(formula, data, filename = NULL, main = NULL, aspect = 'xy', trans = NULL, inv = NULL, colours = NULL, colourkey = TRUE, colourcut = seq(0, 1, length = 11), mincnt = 1, maxcnt = NULL, main.cex = 2.5, xlab.cex = 2.5, ylab.cex = 2.5, xlab.label = NULL, ylab.label = NULL, xlab.col = 'black', ylab.col = 'black', xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 2, yaxis.cex = 2, xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1, grid = FALSE, abline = NULL, abline.front = FALSE, add.axes = FALSE, xbins = 30, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, key = NULL, legend = NULL, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, background.col= 'transparent', xaxis.fontface = 'bold', yaxis.fontface = 'bold') {

	# IMPORTANT NOTE:
	# - the implementation of this function is different from any other functions in the library
	# - it uses do.call() because the maxcnt parameter is passed using missing()
	# - to successfully pass arguments with this problem elsewhere, you need to create a list of arguments
	# - then check if your passed argument !is.null (or whatever you set as the default) and add to the list
	# - then run do.call() on the list
	# - this *may* mess up substitute calls
	# - in future, avoid use of missing()
	
	# WARNING:
	# - if 'maxcnt' is passed, make sure it is not smaller than the actual maximum count (value depends on nbins).
	# - Otherwise, some data may be lost. If you aren't sure what the actual max count is, run this function without
	# - specifying the 'maxcnt' parameter using the desired number of bins. 

	# create the parameter list
	parameter.list <- list(
		formula,
		data,
		panel = function(...) {
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
			if (add.axes) {
				panel.abline(
					h = 0,
					v = 0,
					col.line = "black",
					lty = "dashed",
					lwd = 1.5
					);
				}
			if (abline.front == FALSE) {
				panel.xyplot(
					0,
					0,
					type = 'g',
					col.line = "black",
					abline = abline,
					grid = grid
					);
				}
			panel.hexbinplot(...);
			if (abline.front == TRUE) {
				panel.xyplot(
					0,
					0,
					type = 'g',
					col.line = "black",
					abline = abline,
					grid = grid
					);
				}
			},
		aspect = aspect,
		trans = trans,
		inv = inv,
		xbins = xbins,
		colramp = if (is.null(colours)) { function(n) {LinGray(n, beg = 90, end = 15)} } else { colours },
		colorkey = colourkey,
		colorcut = colourcut,
		mincnt = mincnt,
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
			alternating = FALSE,
			x = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					col = xaxis.col,
					rot = xaxis.rot,
					tck = xaxis.tck,
					limits = xlimits,
					at = xat,
					fontface = xaxis.fontface
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
					limits = ylimits,
					at = yat,
					fontface = yaxis.fontface
					)
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = 2
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
				axis.right = 0.1,
				axis.key.padding = 3,
				key.right = if (colourkey) {1} else {0.1},
				right.padding = right.padding
				),
						panel.background = list(
								col = background.col
								)
			),
		key = key,
		legend = legend,
		grid = grid
		);
	
	# check if the maxcnt parameter has been passed 
	if (!is.null(maxcnt)) { 
		parameter.list[['maxcnt']] <- maxcnt; 
		}
	
	# actually create the plot
	trellis.object <- base::do.call(
		what = 'hexbinplot',
		args = parameter.list
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
