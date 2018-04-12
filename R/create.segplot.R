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
create.segplot <- function(
	formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3,
	xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2,
	xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0, xaxis.lab = TRUE,
	yaxis.lab = TRUE, xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.col = 'black', yaxis.col = 'black',
	xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.rot = 0, yaxis.rot = 0, xaxis.tck = 1,
	yaxis.tck = 1, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, abline.h = NULL, abline.v = NULL,
	abline.lty = 1, abline.lwd = 1, abline.col = 'black', segments.col = 'black', segments.lwd = 1,
	layout = NULL, as.table = FALSE, x.spacing = 0, y.spacing = 0, x.relation = 'same', y.relation = 'same',
	top.padding = 0.5, bottom.padding = 2, right.padding = 1, left.padding = 2, ylab.axis.padding = 0,
	level = NULL, col.regions = NULL, centers = NULL, plot.horizontal = TRUE, draw.bands = FALSE, pch = 16,
	symbol.col = 'black', symbol.cex = 1, add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL,
	xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1,
	axes.lwd = 1, key = NULL, legend = NULL, height = 6, width = 6, size.units = 'in', resolution = 1600,
	enable.warnings = FALSE, description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, inside.legend.auto = FALSE,
        disable.factor.sorting = FALSE 
	) {

        ### store data on mount
        tryCatch({
                        dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if ( !dir.exists(dir.name) ) {
                                dir.create(dir.name);
                                }                        
			funcname <- 'create.segplot';
                        print.to.file(dir.name, funcname, data, filename);
                        },
                warning = function(w) {
                        },
                error = function(e) {
                	});

	### needed to copy in case using variable to define rectangles dimensions
        rectangle.info <- list(
        	xright = xright.rectangle,
                xleft = xleft.rectangle,
                ytop = ytop.rectangle,
                ybottom = ybottom.rectangle
                );

	# add preloaded defaults
        if (preload.default == 'paper') {
                }
        else if (preload.default == 'web') {
                }

	# add some error checking
	if (!plot.horizontal) {
		warning('Be aware that vertical segplots can not be used as the first plot in a multiplot. Consider using create.scatterplot instead.');
		}

	# Now make the actual plot object
	trellis.object <- lattice::levelplot(
		x = formula,
		data,
		prepanel = prepanel.segplot,
		panel = function( abline.local = abline, ...) {

			# add rectangle if requested
			if (add.rectangle) {
				panel.rect(
					xleft = rectangle.info$xleft,
					ybottom = rectangle.info$ybottom,
					xright = rectangle.info$xright,
					ytop = rectangle.info$ytop,
					col = col.rectangle,
					alpha = alpha.rectangle,
					border = NA
					);
				}

			# add bands if requested
			if (!is.logical(draw.bands)) {
				panel.rect(
                                        xleft = data$min[draw.bands],
                                        ybottom = draw.bands - 0.25,
                                        xright = data$max[draw.bands],
                                        ytop = draw.bands + 0.25,
                                        col = segments.col,
                                        alpha = 1,
                                        border = NA
                                        );
				}

			else if (draw.bands == TRUE) {
				panel.rect(
                                        xleft = data$min,
                                        ybottom = seq(0.75, 10, 1),
                                        xright = data$max,
                                        ytop = seq(1.25, 11, 1),
                                        col = segments.col,
                                        alpha = 1,
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
				horizontal = plot.horizontal,
				draw.bands = FALSE,
				...,
				)

			},
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
		between = list(
			x = x.spacing,
			y = y.spacing
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					rot = xaxis.rot,
					limits = xlimits,
					cex = xaxis.cex,
					col = xaxis.col,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
					at = xat,
					relation = x.relation,
					tck = xaxis.tck
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					col = yaxis.col,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
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
				lwd = axes.lwd,
				col = if ('Nature' == style) { 'transparent' } else { 'black' }
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

	if (disable.factor.sorting == TRUE) {
		
		sorting.param <- 'z';

		if (plot.horizontal) {
			if (is.null(trellis.object$y.scales$labels) || (is.logical(trellis.object$y.scales$labels[1]) && trellis.object$y.scales$labels[1]  == TRUE)) {
				default.labels <- unique(as.character(trellis.object$panel.args.common[[sorting.param]]));
				trellis.object$y.scales$labels <- default.labels;
				}
			} 
		else {
			if (is.null(trellis.object$x.scales$labels) || (is.logical(trellis.object$x.scales$labels[1]) && trellis.object$x.scales$labels[1]  == TRUE)) {
                        	default.labels <- unique(as.character(trellis.object$panel.args.common[[sorting.param]])); 
				trellis.object$x.scales$labels <- default.labels;
				trellis.object$x.scales$at <- seq(1, length(default.labels), 1);
				trellis.object$x.limits <- c(0, length(default.labels) + 1);
				}
			}
		
                unique.mapping <- list();
                count <- 1;
                for (x in trellis.object$panel.args.common[[sorting.param]]) {
                        if (is.null(unique.mapping[[as.character(x)]])) {
                                unique.mapping[as.character(x)] <- count;
                                count <- count + 1;
                                }
                        }
                temp.data <- as.character(trellis.object$panel.args.common[[sorting.param]]);
                for (x in 1:length(temp.data)) {
                        temp.data[x] <- as.character(unique.mapping[as.character(trellis.object$panel.args.common[[sorting.param]][[x]])][[1]]);
                        }
                trellis.object$panel.args.common[[sorting.param]] <- as.numeric(temp.data);
	
		}

	if (inside.legend.auto) {

		extra.parameters <- list('x' = trellis.object$panel.args.common$x, 'y' = trellis.object$panel.args.common$y,
			'ylimits' = trellis.object$y.limits, 'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.segplot', filename, trellis.object, height, width, extra.parameters);
                trellis.object$legend$inside$x <- coords[1];
                trellis.object$legend$inside$y <- coords[2];
                }

	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Re-add bottom and left axes
		trellis.object$axis <- function(side, line.col = 'black', ...) {
			# Only draw axes on the left and bottom
			if (side %in% c('bottom', 'left')) {
				axis.default(side = side, line.col = 'black', ...);
				lims <- current.panel.limits();
				panel.abline(h = lims$ylim[1], v = lims$xlim[1]);
				}
			}

		# Ensure sufficient resolution for graphs
		if (resolution < 1200) {
			resolution <- 1200;
			warning('Setting resolution to 1200 dpi.');
			}

		# Other required changes which are not accomplished here
		warning('Nature also requires italicized single-letter variables and en-dashes
			for ranges and negatives. See example in documentation for how to do this.');

		warning('Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend.');
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
