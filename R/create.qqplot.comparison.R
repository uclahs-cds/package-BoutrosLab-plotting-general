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

### FUNCTION TO CREATE QQPLOT COMPARISON ###########################################################
create.qqplot.comparison <- function(x, data = NULL, filename = NULL, aspect = 'fill', prepanel = NULL, grid = FALSE, groups = NULL, main = NULL, xlab.label = NULL, ylab.label = NULL, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xgrid.at = xat, ygrid.at = yat, xaxis.lab = NA, yaxis.lab = NA, xaxis.cex = 1.5, yaxis.cex = 1.5, main.cex = 3, xlab.cex = 2.5, xaxis.fontface = 'bold', xlab.col = 'black', yaxis.fontface = 'bold', ylab.cex = 2.5, ylab.col = 'black', xaxis.log = FALSE, yaxis.log = FALSE, xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', type = 'p', cex = 0.75, pch = 19, col = 'black', lwd = 1, lty = 1, axis.lwd = 2.25, xaxis.tck = 1, yaxis.tck = 1, key = list(text = list(lab = c(''))), legend = NULL, top.padding = 3, bottom.padding = 0.7, left.padding = 0.5, right.padding = 0.1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1) {

	# x should be a formula or a list of data whose length is 2
	if (class(x) == 'list') {

		# create an object to store the data, since the function qq can only handle formula method
		data.to.plot <- data.frame(
			x = c(x[[1]], x[[2]]),
			y = c(rep(1, length(x[[1]])), rep(2, length(x[[2]])))
			);

		formula.to.plot <- y ~ x;
		z <- data.to.plot$x;

		# set x-axis and y-axis label defaults
		# if the label is NULL, then we leave it as blank;
		# if the label is NA, then we use a specific default label.
		if (!is.null(xlab.label) & !is.expression(xlab.label)) { 
			if (is.na(xlab.label)) {
				xlab.label <- "sample one";
				}
			}
		if (!is.null(ylab.label) & !is.expression(ylab.label)) { 
			if (is.na(ylab.label)) {
				ylab.label <- "sample two";
				}
			}
		}

	else { 

		formula.to.plot <- x; 
		data.to.plot <- data;

		# parse the formula to get x-axis and y-axis labels defaults
		parseform <- latticeParseFormula(as.formula(formula.to.plot), data = data.to.plot);
		y <- parseform$left;
		z <- parseform$right;
		y <- as.factorOrShingle(y);

		# if x-axis labels is NA, set the default
		if (!is.null(xlab.label) & !is.expression(xlab.label)) { 
			if (is.na(xlab.label)) {
				# get the name of the first sample from the formula
				if (is.factor(y)) {
					xlab.label <- unique(levels(y))[1];
					}
				else {
					xlab.label <- paste(parseform$left.name, ':', as.character(unique(levels(y)[[1]])));
					}
				}
			}

		# if y-axis labels is NA, set the default
		if (!is.null(ylab.label) & !is.expression(ylab.label)) { 
			if (is.na(ylab.label)) {
				# get the name of the second sample from the formula
					if (is.factor(y)) {
					ylab.label <- unique(levels(y))[2];
					}
					else {
					ylab.label <- paste(parseform$left.name, ':', as.character(unique(levels(y)[[2]])));
					}
				}
			}
		}

	# set main label defaults
	# if the label is NULL, then we leave it as blank;
	# if the label is NA, then we use a specific default label.
	if (!is.null(main) & !is.expression(main)) {
		if (is.na(main)) { 
			main <- 'Q-Q plot'; 
			}
		}

	# create the object to store all the data
	trellis.object <- lattice::qq(
		x = formula.to.plot,
		data = data.to.plot,
		panel = function(type.local = type, groups.local = groups, subscripts, identifier = "qq", ...) {
			
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
						data.vector = z
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = z
						),
					col = trellis.par.get("reference.line")$col
					);
				}
			panel.qq(..., groups = groups.local, subscripts = subscripts, identifier = "qq");
			},
		aspect = aspect,
		grid = grid,
		type = type,
		cex = cex,
		pch = pch,
		col = col,
		lwd = lwd,
		lty = lty,
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
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					limits = xlimits,
					fontface = xaxis.fontface,
					at = xat,
					labels = xaxis.lab,
					log = xaxis.log,
					tck = xaxis.tck,
					alternating = FALSE
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = "fontfamily", 
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					col = yaxis.col,
					limits = ylimits,
					fontface = yaxis.fontface,
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
				lwd = 2.25
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
