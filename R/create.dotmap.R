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

### FUNCTION TO CREATE DOTMAPS #####################################################################
create.dotmap <- function(x, bg.data = NULL, filename = NULL, main = NULL, main.just = 'center',
	main.x = 0.5, main.y = 0.5, pch = 19, pch.border.col = 'black', add.grid = TRUE, xaxis.lab = colnames(x),
	yaxis.lab = rownames(x), xaxis.rot = 0, yaxis.rot = 0, main.cex = 3, xlab.cex = 2, ylab.cex = 2,
	xlab.label = NULL, ylab.label = NULL, xlab.col = 'black', ylab.col = 'black', xlab.top.label = NULL,
	xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center', xlab.top.x = 0.5, xlab.top.y = 0,
	xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.col = 'black', yaxis.col = 'black', xaxis.tck = 1, yaxis.tck = 1,
	axis.top = 1, axis.bottom = 1, axis.left = 1, axis.right = 1, top.padding = 0.1, bottom.padding = 0.7,
	right.padding = 0.1, left.padding = 0.5, key.ylab.padding = 0.1, key = list(text = list(lab = c(''))), legend = NULL, col.lwd = 1.5,
	row.lwd = 1.5, spot.size.function = 'default', spot.colour.function = 'default', na.spot.size = 7,
	na.pch = 4, na.spot.size.colour = 'black', grid.colour = NULL, colour.scheme = 'white', total.colours = 99,
	at = NULL, colour.centering.value = 0, colourkey = FALSE, colourkey.labels.at = NULL,
	colourkey.labels = NULL, colourkey.cex = 1, colour.alpha = 1, bg.alpha = 0.5, fill.colour = 'white',
	key.top = 0.1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	col.colour = 'black', row.colour = 'black', description = 'Created with BoutrosLab.plotting.general',
	add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL,
	ytop.rectangle = NULL, col.rectangle = 'transparent', border.rectangle=NULL, lwd.rectangle = NULL,
	alpha.rectangle = 1, xaxis.fontface = 'bold', yaxis.fontface = 'bold', dot.colour.scheme = NULL,
	style = 'BoutrosLab', preload.default = 'custom', use.legacy.settings = FALSE, remove.symmetric = FALSE, lwd = 2) {

	### needed to copy in case using variable to define rectangles dimensions
	rectangle.info <- list(
		xright = xright.rectangle,
		xleft = xleft.rectangle,
		ytop = ytop.rectangle,
		ybottom = ybottom.rectangle
		);

	if (preload.default == 'paper') {

		}
	else if (preload.default == 'web') {

		}
	data.subset <- TRUE;
	if (remove.symmetric == TRUE) {
		if (ncol(x) != nrow(x)) {
			stop('can only use remove.symmetric with matrices of same length and width');
			}
		data.subset <- c();
		for (i in c(1:nrow(x))) {
			for (j in c(1:nrow(x))) {
				if(j > i) {
					data.subset <- c(data.subset, T);
					}
                                else {
                                        data.subset <- c(data.subset, F);
                                        }

				}
			}
		}        

	x <- as.data.frame(x);
	temp <- x; # 'temp' used for column/row catagorization function

	# determine size/colour functions
	switch(
		as.character(class(spot.size.function)),
		'character' = {
			if (spot.size.function == 'default') {
				spot.size.function <- function(x) { 0.1 + (2 * abs(x)); }
			}
		},
		'numeric' = {
			returnval <- spot.size.function;
			spot.size.function <- function(x) { returnval; }
			}
		);

	if (as.character(class(spot.colour.function) == 'character')) {
		switch(
			spot.colour.function,
			'default' = {
				spot.colour.function <- function(x) {
					colours <- rep('white', length(x));
					colours[sign(x) == -1] <- BoutrosLab.plotting.general::default.colours(2, palette.type = 'dotmap')[1];
					colours[sign(x) ==  1] <- BoutrosLab.plotting.general::default.colours(2, palette.type = 'dotmap')[2];
					return(colours);
					}
				},
			'discrete' = {
				if (length(unique(unlist(x))) > length(dot.colour.scheme)) {
					stop(paste('Not enough colours specified to use discrete function: need at least', length(unique(unlist(x))), 'colours'));
					}
				spot.colour.function <- function(x) {
					unique.values <- unique(x);
					colours <- rep('white', length(x));
					for (i in c(1:length(unique.values))) {
						colours[x == unique.values[i]] <- dot.colour.scheme[i];
						}
					return(colours);
					}
				},
			'columns' = {
				spot.colour.function <- function(x) {
					# The following does not use the parameter 'x' and instead used the variable 'temp'
					no.unique.columns <- length(unique(colnames(temp)));
					no.rows <- length(rownames(temp));
		
					# Checks for repeated colnames and throws an error if there is
					if (no.unique.columns != length(colnames(temp))) {
						stop(paste('Remove repeated column names'));
						}
		
					new.colnames <- seq(1, no.unique.columns, 1);
					colnames(temp) <- new.colnames;
					temp <- stack(temp);
					temp$values <- temp$ind;
					temp$ind <- NULL;
		
					index <- 1;
					colours <- rep('white', no.unique.columns * no.rows);
					for (i in c(1:no.unique.columns)) {
						for (j in c(1:no.rows)) {
							colours[index] <- default.colours(12)[(i %% 12) + 1];
							index <- index + 1;
							}
						}
					return(colours);
					}
				},
			'rows' = {
				spot.colour.function <- function(x) {
					# The following does not use the parameter 'x' and instead used the variable 'temp'
					no.columns <- length(colnames(temp));
					no.unique.rows <- length(unique(rownames(temp)));
		
					# Checks for repeated rownames and throws an error if there is
					if (no.unique.rows != length(rownames(temp))) {
						stop(paste('Remove repeated row names'));
						}
		
					colour.per.column <- c(1:no.unique.rows);
					for (i in 0:no.unique.rows) {
						colour.per.column[i + 1] <- default.colours(12)[(i %% 12) + 1];
						}
		
					temp <- stack(temp);
					temp$ind <- NULL;
		
					index <- 1;
		
					for (i in c(1:no.columns)) {
						for (j in c(1:no.unique.rows)) {
							temp$values[index] <- colour.per.column[j];
							index <- index + 1;
							}
						}
					return(temp);
					}
				}
			);
		}

	# set spot size/colour
	spot.sizes <- spot.size.function(stack(x)$values);
	spot.colours <- spot.colour.function(stack(x)$values);
	spot.border <- pch.border.col;

	# Ensure a bg.data value is provided
	if (!is.null(bg.data)) {
		if (length(colour.scheme) == 1 && colour.scheme == 'white') {
			warning("bg.data is set, but colour.scheme is set to default 'white'. No background colours will be displayed. Changing bg.data to NULL");
			bg.data <- NULL;
			}
		}

	if (is.null(bg.data)) {

		# Set bg.data to x: Ensures a default value of the correct size
		bg.data <- x;

		# Ensure 'fake' bg.data values are never displayed
		if (colourkey) {
			warning('No bg.data set, but colourkey is set to TRUE. Changing colourkey to FALSE');
			colourkey <- FALSE;
			}
		if (length(colour.scheme) != 1 || colour.scheme != 'white') {
			warning("No bg.data set, but colour.scheme is set to non-default value. Changing colour.scheme to 'white'.");
			colour.scheme <- 'white';
			}
		}

	if (is.null(at)) {
		min.value <- min(bg.data - colour.centering.value, na.rm = TRUE);
		max.value <- max(bg.data - colour.centering.value, na.rm = TRUE);
		at <- seq(from = min.value, to = max.value, length.out = total.colours);
		}
	else {
		min.value <- min(at - colour.centering.value, na.rm = TRUE);
		max.value <- max(at - colour.centering.value, na.rm = TRUE);
		min.at <- min(at);
		max.at <- max(at);
		if (min(bg.data, na.rm = TRUE) < min.at) {
			warning(
				paste(
					'min(bg.data) = ',
					min(bg.data, na.rm = TRUE),
					'is smaller than min(at) = ',
					min.at,
					'Clipped data will be plotted'
					)
				);
			bg.data[bg.data < min.at] <- min(at);
			}
		if (max(bg.data, na.rm = TRUE) > max.at) {
			warning(
				paste(
					'max(bg.data) = ',
					max(bg.data, na.rm = TRUE),
					'is greater than max(at) = ',
					max.at,
					'Clipped data will be plotted'
					)
				);
			bg.data[bg.data > max.at] <- max(at);
			}
		total.colours <- max(length(at), total.colours);
		}

	# change alpha value to work for rgb function
	if (bg.alpha <= 1 && bg.alpha >= 0) {
		bg.alpha <- bg.alpha * 255;
		}

	# colour-handling: first handle legacy cases
	if (1 == length(colour.scheme)) {
		if      (colour.scheme == 'RedWhiteBlue')    { colour.scheme <- c('red', 'white', 'blue'); }
		else if (colour.scheme == 'WhiteBlack')      { colour.scheme <- c('white', 'black'); }
		else if (colour.scheme == 'BlueWhiteYellow') { colour.scheme <- c('blue', 'white', 'yellow'); }
		else if (colour.scheme == 'white')           { colour.scheme <- c('white', 'white'); }
		else {
			warning(paste('Unknown colour scheme:', colour.scheme));
			return(0);
			}
		}

	# one-sided colour schemes
	if (2 == length(colour.scheme)) {
		colour.function <- colorRamp(colour.scheme, space = 'Lab');
		my.palette <- rgb(colour.function(seq(0, 1, 1 / total.colours) ^ colour.alpha), alpha = bg.alpha, maxColorValue = 255);
		}

	# two-sided colour schemes
	else if (3 == length(colour.scheme)) {

		# warn user if they use three-colour scheme with one-sided data
		is.twosided <- sign(min.value) != sign(max.value);
		if (!is.twosided) {
			warning('Using a three-colour scheme with one-sided data is not advised!');
			}

		# create colour scheme
		colour.function.low <- colorRamp(colour.scheme[1:2], space = 'Lab');
		colour.function.high <- colorRamp(colour.scheme[2:3], space = 'Lab');

		# The number of negative colours is based on the fraction of the range that's below the center value
		# The number of positive colours is based on the number of negatives
		# Leave one colour free for the center value
		neg.colours <- min.value / (max.value - min.value) * (total.colours - 1);
		neg.colours <- ceiling(abs(neg.colours));
		pos.colours <- total.colours - neg.colours - 1;

		# There is potential for colour allocation to go wrong when:
		#	1) There is one-sided data
		#	2) The colour-centering is at zero
		#	3) A three-colour scheme is requested
		# Try to automatically detect this case and provide a fix
		if (neg.colours < 1 | pos.colours < 1) {
			warning('Colour allocation scheme failed, moving to a default method');
			neg.colours <- round(total.colours / 2);
			pos.colours <- round(total.colours / 2);
			}

		# create colour palette
		my.palette <- c(
			rgb(colour.function.low(seq(0, 1, 1 / neg.colours) ^ colour.alpha), alpha = bg.alpha, maxColorValue = 255),
			colour.scheme[2],
			rgb(colour.function.high(seq(0, 1, 1 / pos.colours) ^ (1 / colour.alpha)), alpha = bg.alpha, maxColorValue = 255)
			);
		}

	else {
		my.palette <- c();
		for (n in 1:length(colour.scheme)) {
			colour.function <- colorRamp(c('white', colour.scheme[n]), space = 'Lab');
			my.palette <- c(my.palette, rgb(colour.function(1 ^ colour.alpha), alpha = bg.alpha, maxColorValue = 255));
			}
		}

	# format bg.data
	bg.data <- as.data.frame(bg.data);

	# constructing coordinate system
	# note that we're forcing the 'natural' ordering of rows here
	y.coords <- rep(nrow(x):1, ncol(x));
	x.coords <- c();

	for (i in 1:ncol(x)) { x.coords <- c(x.coords, rep(i, nrow(x))); }

	bg.data <- data.frame(
		x = x.coords,
		y = y.coords,
		freq = stack(bg.data)$values
		);

	if (colourkey) {
		colourkey <- list(
			size = 1,
			space = 'bottom',
			width = 1.25,
			height = 1.0,
			labels = list(
				cex = colourkey.cex,
				at = colourkey.labels.at,
				labels = colourkey.labels
				),
			tick.number = 3
			);
		}

	if (!is.null(grid.colour)) {
		row.colour <- grid.colour;
		col.colour <- grid.colour;
		cat(paste0('CAUTION: grid.colour is DEPRECATED!  Use row.colour/col.colour. Using: ', grid.colour, '\n'));
		}

	if (any(is.na(x))) {
		tmp.pch <- pch;
		pch[is.na(x)] <- na.pch;
		pch[!is.na(x)] <- tmp.pch;
		spot.colours[is.na(x)] <- na.spot.size.colour;
		spot.sizes[is.na(x)] <- na.spot.size;
		rm(tmp.pch);
		}

	if (is.null(lwd.rectangle) & !is.null(border.rectangle)) {
		lwd.rectangle <- 1;
		}

	trellis.object <- lattice::levelplot(
		freq ~ x * y,
		bg.data,
		subset = data.subset,
		panel = function(...) {

			panel.fill(col = fill.colour);
			panel.levelplot(...);

			# add rectangle if requested
	#		if (add.rectangle) {
	#			panel.rect(
	#				xleft = xleft.rectangle,
	#				ybottom = ybottom.rectangle,
	#				xright = xright.rectangle,
	#				ytop = ytop.rectangle,
	#				col = col.rectangle,
	#				alpha = alpha.rectangle,
	#				border = ifelse(is.null(border.rectangle),NA,border.rectangle),
	#				lwd = lwd.rectangle
	#				);
	#			}

			# add grid if requested
			if (add.grid) {
				if(remove.symmetric == TRUE) {
					for(i in c(1:max(bg.data$y))) {
						panel.lines(x = c(0,max(bg.data$x) - (i)) + 0.5, y = i + 0.5, col=col.colour, lwd = col.lwd);
						}
					for(i in c(1:max(bg.data$x))) {
						panel.lines(x = i + 0.5, y = c(0,max(bg.data$y) - (i)) + 0.5, col=row.colour, lwd = row.lwd);
						}
					}
				else {
					panel.abline(
						h = min(bg.data$y):max(bg.data$y) - 0.5,
						v = 0,
						col.line = row.colour,
						lwd = row.lwd
						);
					panel.abline(
						v = min(bg.data$x):max(bg.data$x) - 0.5,
						h = 0,
						col.line = col.colour,
						lwd = col.lwd
						);
					}
				}
			if (add.rectangle) {
				panel.rect(
					xleft = rectangle.info$xleft,
					ybottom = rectangle.info$ybottom,
					xright = rectangle.info$xright,
					ytop = rectangle.info$ytop,
					col = col.rectangle,
					alpha = alpha.rectangle,
					border = ifelse(is.null(border.rectangle), NA, border.rectangle),
					lwd = lwd.rectangle
					);
				}

			# NOTE: different ways to handle border and fill for pch < 21 and pch >= 21
			panel.xyplot(
				type = 'p',
				cex = spot.sizes,
				pch = pch,
				col = mapply(
					function(pch, spot.colours, spot.border) {
						if (pch %in% 0:20) { return(spot.colours); } else
						if (pch %in% 21:25) { return(spot.border); }
						},
					pch, spot.colours = spot.colours, spot.border = spot.border
					),
				fill = mapply(
					function(pch, spot.colours) {
						if (pch %in% 0:20) { NA; } else
						if (pch %in% 21:25) { return(spot.colours); }
						},
					pch, spot.colours = spot.colours
					),
				key = key,
				legend = legend,
				...
				);
			},
		at = at,
		key = key,
		legend = legend,
		col.regions = my.palette,
		colorkey = colourkey,
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
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
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				cex = xlab.cex,
				col = xlab.col
				)
			),
		xlab.top = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			use.legacy.settings = use.legacy.settings || ('Nature' == style),
			add.to.list = list(
				label = xlab.top.label,
				cex = xlab.top.cex,
				col = xlab.top.col,
				fontface = if ('Nature' == style) {'plain'} else {'bold'},
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
				fontface = if ('Nature' == style) {'plain'} else ('bold'),
				cex = ylab.cex,
				col = ylab.col
				)
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					tck = xaxis.tck,
					limits = c( min(bg.data$x, na.rm = TRUE) - 0.5, max(bg.data$x, na.rm = TRUE) + 0.5 ),
					at = min(bg.data$x, na.rm = TRUE):max(bg.data$x, na.rm = TRUE),
					fontface = if ('Nature' == style) {'plain'} else (xaxis.fontface)
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					labels = rev(yaxis.lab),
					cex = yaxis.cex, # necessary because we forcing the 'natural' ordering of rows when making bg.data
					rot = yaxis.rot,
					col = yaxis.col,
					tck = yaxis.tck,
					limits = c( min(bg.data$y, na.rm = TRUE) - 0.5, max(bg.data$y, na.rm = TRUE) + 0.5 ),
					at = min(bg.data$y, na.rm = TRUE):max(bg.data$y, na.rm = TRUE),
					fontface = if ('Nature' == style) {'plain'} else (yaxis.fontface)
					)
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = lwd,
				col = if(remove.symmetric == TRUE) { 'transparent'; } else { 'black'; }
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3} else { 1 },
				main.key.padding = 0.1,
				key.top = key.top,
				key.axis.padding = 0.1,
				axis.top = axis.top,
				axis.bottom = axis.bottom,
				axis.xlab.padding = 1,
				xlab = if (is.null(xaxis.lab)) {0.1} else {1},
				xlab.key.padding = 0.5,
				key.bottom = 1,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 1,
				key.ylab.padding = key.ylab.padding,
				ylab = if (is.null(yaxis.lab)) {0.1} else {1},
				ylab.axis.padding = 1,
				axis.left = axis.left,
				axis.right = axis.right,
				axis.key.padding = 0.1,
				key.right = 1,
				right.padding = right.padding
				)
			)
		);
	
	if (remove.symmetric == TRUE) {
		# Re-add bottom and left axes
                trellis.object$axis <- function(side, line.col = 'black', ...) {
                        # Only draw axes on the left and bottom
                        if (side %in% c('bottom', 'left')) {
                                axis.default(side = side, line.col = 'black', ...);
                                lims <- current.panel.limits();
                                panel.abline(h = lims$ylim[1], v = lims$xlim[1], lwd = lwd);
                                }
                        }
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
