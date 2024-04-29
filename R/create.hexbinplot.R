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
create.hexbinplot <- function(
	formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5,
	main.cex = 3, aspect = 'xy', trans = NULL, inv = NULL, colour.scheme = NULL, colourkey = TRUE,
	colourcut = seq(0, 1, length = 11), mincnt = 1, maxcnt = NULL, xbins = 30, legend.title = NULL, 
	xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2,
	xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, xlimits = NULL,
	ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA, yaxis.lab = NA,	xaxis.cex = 1.5, yaxis.cex = 1.5,
	xaxis.rot = 0, yaxis.rot = 0, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1,
	xaxis.fontface = 'bold', yaxis.fontface = 'bold', layout = NULL, as.table = FALSE, x.relation = 'same',
	y.relation = 'same', x.spacing = 0, y.spacing = 0, strip.col = 'white', strip.cex = 1, strip.fontface = 'bold',
	add.grid = FALSE, abline.h = NULL, abline.v = NULL, abline.lty = NULL, abline.lwd = NULL,
	abline.col = 'black', abline.front = FALSE, add.xyline = FALSE, xyline.col = 'black', xyline.lwd = 1,
	xyline.lty = 1, add.curves = FALSE, curves.exprs = NULL, curves.from = min(data, na.rm = TRUE),
	curves.to = max(data, na.rm = TRUE), curves.col = 'black', curves.lwd = 2, curves.lty = 1,
	add.text = FALSE, text.labels = NULL, text.x = NULL, text.y = NULL, text.col = 'black',	text.cex = 1,
	text.fontface = 'bold', add.axes = FALSE, top.padding = 0.1, bottom.padding = 0.7, left.padding = 0.5,
	right.padding = 0.1, add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL,
	xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1,
	background.col = 'transparent',	key = NULL, legend = NULL, height = 6, width = 6, size.units = 'in',
	resolution = 1600, enable.warnings = FALSE, description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, inside.legend.auto = FALSE
	) {

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

	### needed to copy in case using variable to define rectangles dimensions
	rectangle.info <- list(
		xright = xright.rectangle,
		xleft = xleft.rectangle,
		ytop = ytop.rectangle,
		ybottom = ybottom.rectangle
		);

	text.info <- list(
		labels = text.labels,
		x = text.x,
		y = text.y,
		col = text.col,
		cex = text.cex,
		fontface = text.fontface
		);

        out <- prep.axis(
                at = xat,
                data = unlist(data[toString(formula[[3]])]),
                which.arg = 'xat'
                );
        if (is.list(out)) {
                data[toString(formula[[3]])] <- out$x;
                xat <- out$at;
                xaxis.lab <- out$axis.lab;
                }

        out <- prep.axis(
                at = yat,
                data = unlist(data[toString(formula[[2]])]),
                which.arg = 'yat'
                );
        if (is.list(out)) {
                data[toString(formula[[2]])] <- out$x;
                yat <- out$at;
                yaxis.lab <- out$axis.lab;
                }

	# check class of conditioning variable
	if ('|' %in% all.names(formula)) {
		variable <- sub('^\\s+', '', unlist(strsplit(toString(formula[length(formula)]), '\\|'))[2]);
		if (variable %in% names(data)) {
			cond.class <- class(data[, variable]);
			if (cond.class %in% c('integer', 'numeric')) {
				warning(
					'Numeric values detected for conditional variable. If text labels are desired, please convert conditional variable to character.'
					);
				}
			rm(cond.class);
			}
		}

	# add preloaded defaults
	if (preload.default == 'paper') {
		}
	else if (preload.default == 'web') {
		}

	# update/modify legend title if desired
	if (!is.null(legend.title) & !is.null(key)) {
		stop('ERROR: cannot modify default title while additional key is being supplied; please use legend to specify additional keys.');
		}

	if (!is.null(legend.title) & is.null(key)) {
		if (!is.list(legend.title)) {
			legend.title <- list(lab = legend.title, x = 1, y = 1.1);
			}
		else if (is.null(legend.title$lab) || is.null(legend.title$x) || is.null(legend.title$y)) {
			stop('ERROR: if supplying modified legend title as a list, must provide all of list(lab, x, y) components.');
			}

		key <- list(
			text = list(
				lab = legend.title$lab,
				cex = 1.5
				),
			x = legend.title$x,
			y = legend.title$y
			);
		}

	# fill in the defined parameters
	parameter.list <- list(
		formula,
		data,
		panel = function(...) {

			# add rectangle if requested
			if (add.rectangle) {
				panel.rect(
					xleft   = rectangle.info$xleft,
					ybottom = rectangle.info$ybottom,
					xright  = rectangle.info$xright,
					ytop    = rectangle.info$ytop,
					col     = col.rectangle,
					alpha   = alpha.rectangle,
					border  = NA
					);
				}

			# if axes are requested
			if (add.axes) {
				panel.abline(
					h	 = 0,
					v	 = 0,
					col.line = 'black',
					lty      = 'dashed',
					lwd      = 1.5
					);
				}

			# if abline should be placed behind
			# create and add abline to empty plot
			if (abline.front == FALSE) {
				panel.xyplot(
					0,
					0,
					type     = 'g',
					col.line = 'black',
					grid     = add.grid
					);

				panel.abline(h = abline.h, lty = abline.lty, lwd = abline.lwd, col = abline.col);
				panel.abline(v = abline.v, lty = abline.lty, lwd = abline.lwd, col = abline.col);

				}

			# otherwise, if abline is to be placed in front
			# create hexbin plot
			panel.hexbinplot(...);

			# and then add abline
			if (abline.front == TRUE) {
				panel.xyplot(
					0,
					0,
					type     = 'g',
					col.line = 'black',
					grid     = add.grid
					);

				panel.abline(h = abline.h, lty = abline.lty, lwd = abline.lwd, col = abline.col);
				panel.abline(v = abline.v, lty = abline.lty, lwd = abline.lwd, col = abline.col);

				}

			# if xy line is requested
			if (add.xyline) {
				panel.abline(
					a   = 0,
					b   = 1,
					lwd = xyline.lwd,
					lty = xyline.lty,
					col = xyline.col
					);
				}

			# if requested, add curve segments
			if (add.curves) {

				if (length(curves.exprs) > 1) {
					if (1 == length(curves.from)) { curves.from <- rep(curves.from, length(curves.exprs)); }
					if (1 == length(curves.to))   { curves.to   <- rep(curves.to,   length(curves.exprs)); }
					if (1 == length(curves.col))  { curves.col  <- rep(curves.col,  length(curves.exprs)); }
					if (1 == length(curves.lwd))  { curves.lwd  <- rep(curves.lwd,  length(curves.exprs)); }
					if (1 == length(curves.lty))  { curves.lty  <- rep(curves.lty,  length(curves.exprs)); }
					}

				for (i in 1:length(curves.exprs)) {
					with(
						data = new.env(),
						expr = panel.curve(
							expr = curves.exprs[[i]](x),
							from = curves.from[i],
							to   = curves.to[i],
							col  = curves.col[i],
							lwd  = curves.lwd[i],
							lty  = curves.lty[i]
							)
						);
					}
				}

			# Add text to plot
			if (add.text) {
				panel.text(
					x	 = text.info$x,
					y	 = text.info$y,
					labels   = text.info$labels,
					col      = text.info$col,
					cex      = text.info$cex,
					fontface = text.info$fontface
					);
				}
			},
		aspect = aspect,
		trans = trans,
		inv = inv,
		xbins = xbins,
		colramp = if (is.null(colour.scheme)) { function(n) { LinGray(n, beg = 90, end = 15) } } else { colour.scheme },
		colorkey = colourkey,
		colorcut = colourcut,
		mincnt = mincnt,
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
				cex = main.cex,
				just = main.just,
				x = main.x,
				y = main.y
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.label,
				cex = xlab.cex,
				col = xlab.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' }
				)
			),
		xlab.top = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.top.label,
				cex = xlab.top.cex,
				col = xlab.top.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' },
				just = xlab.top.just,
				x = xlab.top.x,
				y = xlab.top.y
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = ylab.label,
				cex = ylab.cex,
				col = ylab.col,
				fontface = if ('Nature' == style) { 'plain' } else { 'bold' }
				)
			),
		scales = list(
			alternating = FALSE,
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					col = xaxis.col,
					rot = xaxis.rot,
					tck = xaxis.tck,
					limits = xlimits,
					relation = x.relation,
					at = xat,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface }
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					rot = yaxis.rot,
					tck = yaxis.tck,
					limits = ylimits,
					relation = y.relation,
					at = yat,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface }
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
			axis.line = list(lwd = 2),
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
				axis.right = 0.1,
				axis.key.padding = 3,
				key.right = if (colourkey) { 1 } else { 0.1 },
				right.padding = right.padding
				),
			panel.background = list(
				col = background.col
				),
			strip.background = list(
				col = strip.col
				)
			),
		par.strip.text = list(
			cex = strip.cex,
			fontface = strip.fontface
			),
		key = key,
		legend = legend,
		grid = add.grid
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

	# update/modify legend title if desired
	if (!is.null(legend.title)) {
		trellis.object$legend$right$args$cex.title = 0;
		}

	if (inside.legend.auto) {
		extra.parameters <- list('x' = trellis.object$panel.args[[1]]$x, 'y' = trellis.object$panel.args[[1]]$y, 'ylimits' = trellis.object$y.limits,
			'xlimits' = trellis.object$x.limits, 'xbins' = xbins, 'aspect.ratio' = trellis.object$panel.args.common$.aspect.ratio);
		coords <- c();
		coords <- .inside.auto.legend('create.hexbinplot', filename, trellis.object, height, width, extra.parameters);
		trellis.object$legend$inside$x <- coords[1];
		trellis.object$legend$inside$y <- coords[2];
		}

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Ensure sufficient resolution for graphs
		if (resolution < 1200) {
			resolution <- 1200;
			warning('Setting resolution to 1200 dpi.');
			}

		# Other required changes which are not accomplished here
		warning('Nature also requires italicized single-letter variables and en-dashes
			for ranges and negatives. See example in documentation for how to do this.');

		warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend');
		}

	# Otherwise use the BL style if requested
	else if ('BoutrosLab' == style) {
		# Nothing happens
		}

	# if neither of the above is requested, give a warning
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
