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
create.violinplot <- function(
	formula, data, filename = NULL, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5,
	main.cex = 3, xlab.label = tail(sub('~', '', formula[-2]), 1), ylab.label = tail(sub('~', '', formula[-3]), 1),
	xlab.cex = 2, ylab.cex = 2, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL,
	xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0,
	xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.col = 'black',
	yaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.rot = 0, yaxis.rot = 0,
	xaxis.tck = c(1, 0), yaxis.tck = c(1, 1), xlimits = NULL, xat = TRUE, ylimits = NULL, yat = TRUE,
    col = 'black', lwd = 1, border.lwd = 1, bandwidth = 'nrd0', bandwidth.adjust = 1, 
    extra.points = NULL, extra.points.pch = 21, extra.points.col = 'white', extra.points.border = 'black', extra.points.cex = 1,
    start = NULL, end = NULL, scale = FALSE, plot.horizontal = FALSE, top.padding = 0.1, bottom.padding = 0.7,
	left.padding = 0.5, right.padding = 0.3, key = NULL, legend = NULL, add.rectangle = FALSE,
	xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL,
	col.rectangle = 'transparent', alpha.rectangle = 1, height = 6, width = 6, resolution = 1600,
	size.units = 'in', enable.warnings = FALSE, description = 'Created with BoutrosLab.plotting.general',
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, disable.factor.sorting = FALSE,
	strip.col = 'white', strip.cex = 1,	strip.fontface = 'bold', layout = NULL
    ) {

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

    # Temp function to allow differential violin colour filling
    # panel.violin() author has been emailed about issue in the
    # original function preventing differential colouring.
    .panel.violin.mod <- function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
        horizontal = TRUE, alpha = plot.polygon$alpha, border = plot.polygon$border,
        lty = plot.polygon$lty, lwd = plot.polygon$lwd, col = plot.polygon$col,
        varwidth = FALSE, bw = bandwidth, adjust = bandwidth.adjust, kernel = NULL,
        window = NULL, width = NULL, n = 50, from = NULL, to = NULL,
        cut = NULL, na.rm = TRUE, ..., identifier = 'violin') {

        if (all(is.na(x) | is.na(y))) { return(); }

        x <- as.numeric(x);
        y <- as.numeric(y);
        plot.polygon <- trellis.par.get('plot.polygon');
        darg <- list();
        darg$bw <- bw;
        darg$adjust <- adjust;
        darg$kernel <- kernel;
        darg$window <- window;
        darg$width <- width;
        darg$n <- n;
        darg$from <- from;
        darg$to <- to;
        darg$cut <- cut;
        darg$na.rm <- na.rm;

        my.density <- function(x) {
            ans <- try(
                do.call('density', c(list(x = x), darg)),
                silent = TRUE
                );

            if (inherits(ans, 'try-error')) {
                list(x = rep(x[1], 3), y = c(0, 1, 0));
                }
            else { ans; }
            }

        numeric.list <- if (horizontal) { split(x, factor(y)); } else { split(y, factor(x)); }
        levels.fos <- as.numeric(names(numeric.list));

        # check colours are appropriate length
        if (length(col) < length(levels.fos)) { col <- rep(col, length(levels.fos)); }

        d.list <- lapply(numeric.list, my.density);
        dx.list <- lapply(d.list, '[[', 'x');
        dy.list <- lapply(d.list, '[[', 'y');
        max.d <- sapply(dy.list, max);
        if (varwidth) { max.d[] <- max(max.d); }
        cur.limits <- current.panel.limits();
        xscale <- cur.limits$xlim;
        yscale <- cur.limits$ylim;
        height <- box.width;

        ## Modified from methods::hasArg.
        hasGroupNumber <- function() {
            aname <- 'group.number';
            fnames <- names(formals(sys.function(sys.parent())));
            if (is.na(match(aname, fnames))) {
                if (is.na(match('...', fnames))) { FALSE; }
                else {
                    dots.call <- eval(quote(substitute(list(...))), sys.parent());
                    !is.na(match(aname, names(dots.call)));
                    }
                }
            else { FALSE; }
            }

        if (hasGroupNumber()) {
            group <- list(...)$group.number;
            }
        else { group <- 0; }

        if (horizontal) {
            for (i in seq_along(levels.fos)) {
                if (is.finite(max.d[i])) {
                    pushViewport(
                        viewport(
                            y = unit(levels.fos[i], 'native'),
                            height = unit(height, 'native'),
                            yscale = c(max.d[i] * c(-1, 1)),
                            xscale = xscale
                            )
                        );

                    grid.polygon(
                        x = c(dx.list[[i]], rev(dx.list[[i]])),
                        y = c(dy.list[[i]], -rev(dy.list[[i]])),
                        default.units = 'native',
                        name = trellis.grobname(
                            identifier,
                            type = 'panel',
                            group = group
                            ),
                        gp = gpar(fill = col[i], col = border, lty = lty, lwd = lwd, alpha = alpha)
                        );

                    popViewport();
                    }
                }
            }
        else {
            for (i in seq_along(levels.fos)) {
                if (is.finite(max.d[i])) {
                    pushViewport(
                        viewport(
                            x = unit(levels.fos[i], 'native'),
                            width = unit(height, 'native'),
                            xscale = c(max.d[i] * c(-1, 1)),
                            yscale = yscale
                            )
                        );

                    grid.polygon(
                        y = c(dx.list[[i]], rev(dx.list[[i]])),
                        x = c(dy.list[[i]], -rev(dy.list[[i]])),
                        default.units = 'native',
                        name = trellis.grobname(
                            identifier,
                            type = 'panel',
                            group = group
                            ),
                        gp = gpar(fill = col[i], col = border, lty = lty, lwd = lwd, alpha = alpha)
                        );

                    popViewport();
                    }
                }
            }

        invisible();
        }

    # Now make the actual plot object
    trellis.object <- lattice::bwplot(
        formula,
        data,
        panel = function(from = start, to = end, varwidth = scale, ...) {

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

            # update the plot parameters
            if (is.null(from) || is.null(to)) {
                .panel.violin.mod(varwidth = varwidth, ...);
                }
            else {
                .panel.violin.mod(from = from, to = to, varwidth = varwidth, ...);
                }

            # add extra points if requested
            if (!is.null(extra.points)) {
                for (i in 1:length(extra.points)) {

                    if (is.na(extra.points.pch[i])) { extra.points.pch[i] <- extra.points.pch[1]; }
                    if (is.na(extra.points.col[i])) { extra.points.col[i] <- extra.points.col[1]; }
                    if (is.na(extra.points.cex[i])) { extra.points.cex[i] <- extra.points.cex[1]; }
                    if (is.na(extra.points.border[i])) { extra.points.border[i] <- extra.points.border[1]; }

					for (j in 1:length(extra.points[[i]])) {
						if (!is.na(extra.points[[i]][j])) {
							if (plot.horizontal) {
								xyplot.x <- extra.points[[i]][j];
								xyplot.y <- j;
								}
							else {
								xyplot.x <- j;
								xyplot.y <- extra.points[[i]][j];
								}
							panel.xyplot(
								x = xyplot.x,
								y = xyplot.y,
								pch = extra.points.pch[i],
								col = if (extra.points.pch[i] %in% 0:20) { extra.points.col[i]; } else if
									(extra.points.pch[i] %in% 21:25) { extra.points.border[i]; },
								fill = if (extra.points.pch[i] %in% 0:20) { NA; } else if
									(extra.points.pch[i] %in% 21:25) { extra.points.col[i]; },
								cex = extra.points.cex[i]
								);
							}
						}
					}
				}
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
		scales = list(
			lwd = 1,
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
                    limits = xlimits,
                    at = xat,
					rot = xaxis.rot,
					col = xaxis.col,
					tck = xaxis.tck,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface }
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					limits = ylimits,
					at = yat,
					rot = yaxis.rot,
					col = yaxis.col,
					tck = yaxis.tck,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface }
					)
				)
			),
		layout = layout,
		par.settings = list(
			axis.line = list(
				lwd = lwd,
				col = if ('Nature' == style) { 'transparent' } else { 'black' }
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
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = 1,
				between = 1,
				axis.right = 1,
				axis.key.padding = 0.1,
				right.padding = right.padding
				),
			strip.background = list(
				col = strip.col
				),
			box.dot = list(
				pch = 19,
				col = '#000000',
				lty = 1
				),
			box.rectangle = list(
				lwd = 3,
				col = '#000000',
				lty = 1
				),
			box.umbrella = list(
				lwd = 2,
				col = '#000000',
				lty = 1
				),
			plot.symbol = list(
				col = '#000000',
				pch = 19,
				cex = 0.5
				),
			plot.polygon = list(
				col = col,
				lwd = border.lwd
				)
			),
		pch = '|',
		pretty = TRUE,
		horizontal = plot.horizontal,
		par.strip.text = list(
			cex = strip.cex,
			fontface = strip.fontface
			),
		key = key,
		legend = legend
		);

    if (disable.factor.sorting == TRUE) {

        sorting.param <- '';

        if (plot.horizontal) {
            sorting.param <- 'y';
            if (is.null(trellis.object$y.scales$labels) || (is.logical(trellis.object$y.scales$labels[1]) && trellis.object$y.scales$labels[1]  == TRUE)) {
                default.labels <- unique(as.character(trellis.object$panel.args[[1]][[sorting.param]]));
                trellis.object$y.scales$labels <- default.labels;
                }
            }
        else {
            sorting.param <- 'x';
            if (is.null(trellis.object$x.scales$labels) || (is.logical(trellis.object$x.scales$labels[1]) && trellis.object$x.scales$labels[1]  == TRUE)) {
                            default.labels <- unique(as.character(trellis.object$panel.args[[1]][[sorting.param]]));
                trellis.object$x.scales$labels <- default.labels;
                }
            }

                unique.mapping <- list();
                count <- 1;
                for (x in trellis.object$panel.args[[1]][[sorting.param]]) {
                        if (is.null(unique.mapping[[as.character(x)]])) {
                                unique.mapping[as.character(x)] <- count;
                                count <- count + 1;
                                }
                        }
                temp.data <- as.character(trellis.object$panel.args[[1]][[sorting.param]]);
                for (x in 1:length(temp.data)) {
                        temp.data[x] <- as.character(unique.mapping[as.character(trellis.object$panel.args[[1]][[sorting.param]][[x]])][[1]]);
                        }
                trellis.object$panel.args[[1]][[sorting.param]] <- as.numeric(temp.data);

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
