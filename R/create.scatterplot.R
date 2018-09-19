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

### inside.ellipse #################################################################################
#
# PURPOSE
# Produces an NxM matrix of booleans corresponding to N data points (data.x and data.y)
# being inside M ellipses (ellipse.center.x, ellipse.radii.x, ellipse.center.y, ellipse.radii.y).
#
# INPUT
# ellipse.center.x:	is an numeric defining the x coordinate(s) of the ellipse center
# ellipse.center.y:	is an numeric defining the y coordinate(s) of the ellipse center
# ellipse.radii.x:	is an numeric defining the radius(radii) lengths along the x axis
# ellipse.radii.y:	is an numeric defining the radius(radii) lengths along the y axis
# data.x:		the numeric data x coordinates of the data points to check if they are inside the ellipse
# data.y:		the numeric data y coordinates of the data points to check if they are inside the ellipse
#
# OUTPUT
# An NxM matrix of booleans where N defines all the data points and M are all the possible ellipses.
#
####################################################################################################
inside.ellipse <- function(ellipse.center.x, ellipse.center.y, ellipse.radii.x, ellipse.radii.y, data.x, data.y) {

	### PARAMETER CHECK ########################################################################

	# ensure input variables are numeric, otherwise throw a warning.
	if (!is.numeric(ellipse.center.x) || !is.numeric(ellipse.radii.x) || !is.numeric(ellipse.center.y) ||
		!is.numeric(ellipse.radii.y) || !is.numeric(data.x) || !is.numeric(data.y)) {
		warning('In inside.ellipse: input variables are not all numeric.');
		}

	# ensure input variable are vectors, otherwise throw a warning.
	if (!is.vector(ellipse.center.x) || !is.vector(ellipse.radii.x) || !is.vector(ellipse.center.y) ||
		!is.vector(ellipse.radii.y) || !is.vector(data.x) || !is.vector(data.y)) {
		warning('In inside.ellipse: input variables are not all vectors.');
		}

	# ensure the lengths of the variables defining all the ellipses are the same
	if (!(
			length(ellipse.center.x) == length(ellipse.center.y) &&
			length(ellipse.radii.x) == length(ellipse.radii.y) &&
			length(ellipse.center.x) == length(ellipse.radii.x)
		)) {
		warning('In inside ellipse: The length of ellipse.center.x, ellipse.radii.x, ellipse.center.y and ellipse.radii.y are not all the same.');
		}

	# ensure the lengths of the data points are the same
	if (!length(data.x) == length(data.y)) {
		warning('In inside.ellipse: The length of data.x and data.y are different.');
		}

	### ELLIPSE CALCULATION ####################################################################
	# for every element in data.x and every element in ellipse.center.x, compute the cartesian
	# subtraction, such that the resulting variable is a matrix with row i representing
	# the difference between the ith data.x and all the ellipse.center.x
	difference.x <- outer(
		X = data.x,
		Y = ellipse.center.x,
		FUN = '-'
		);

	# for every element in data.y and every element in ellipse.center.y, compute the cartesian
	# subtraction, such that the resulting variable is a matrix with row i representing
	# the difference between the ith data.y and all the ellipse.center.y
	difference.y <- outer(
		X = data.y,
		Y = ellipse.center.y,
		FUN = '-'
		);

	# a matrix of booleans corresponding to the outcome of data points data.x and data.y being inside ellipses
	# defined by the 4 ellipse variables (ellipse.center.x, ellipse.center.y, ellipse.radii.x, ellipse.radii.y).
	# Currently this matrix has rows as the data.points and columns as the ellipses. Thus at row i and column j
	# We have data.point i and ellipse j.
	inside.ellipse.outcome <- ( (difference.x / ellipse.radii.x) ^ 2) + ( (difference.y / ellipse.radii.y) ^ 2) <= 1;

	return(inside.ellipse.outcome);
	}

### inside.rectangle ###############################################################################
#
# PURPOSE
# Produces an NxM matrix of booleans corresponding to N data points (data.x and data.y)
# being inside M ellipses (ellipse.center.x, ellipse.radii.x, ellipse.center.y, ellipse.radii.y).
#
# INPUT
# rectangle.center.x:	is an numeric defining the x coordinate(s) of the rectangle center
# rectangle.center.y:	is an numeric defining the y coordinate(s) of the rectangle center
# rectangle.width:	is an numeric defining the width (the length of the rectangle along the x axis) of the rectangle
# rectangle.height:	is an numeric defining the height (the length of the rectangle along the y axis) of the rectangle
# data.x:		the numeric data x coordinates of the data points to check if they are inside the ellipse
# data.y:		the numeric data y coordinates of the data points to check if they are inside the ellipse
#
# OUTPUT
# An NxM matrix of booleans where N defines all the data points and M are all the possible rectangles.
#
####################################################################################################
inside.rectangle <- function(rectangle.center.x, rectangle.center.y, rectangle.width, rectangle.height, data.x, data.y) {

	### PARAMETER CHECK ########################################################################
	# ensure all variables are numeric, otherwise throw a warning
	if (!is.numeric(rectangle.center.x) || !is.numeric(rectangle.center.y) || !is.numeric(rectangle.width)
		|| !is.numeric(rectangle.height) || !is.numeric(data.x) || !is.numeric(data.y)) {
		warning('Variables passed to inside.rectangle are not all numeric.');
		}

	# ensure all variables are vectors, otherwise throw a warning
	if (!is.vector(rectangle.center.x) || !is.vector(rectangle.center.y) || !is.vector(rectangle.width)
		|| !is.vector(rectangle.height) || !is.vector(data.x) || !is.vector(data.y)) {
		warning('Variables passed to inside.rectangle are not all vectors.');
		}

	# ensure all variables defining the rectangles are of the same length
	if (length(rectangle.center.x) != length(rectangle.center.y) || length(rectangle.height) != length(rectangle.width)) {
		warning('Variables which define the rectangle in inside.rectangle are not all of the same length.');
		}

	# ensure all variables defining the data points are of the same length
	if (length(data.x) != length(data.y)) {
		warning('Variables which define the data points in inside.rectangle are no all of the same length.');
		}

	### RECTANGLE CALCULATION ##################################################################
	# replicate values in rectangle.center.x by the length of data.x
	# this is done to vectorize the boolean calculation and to avoid matrices.
	# in general we are computing the cartesian outcome of all defined rectangles and all
	# data points, thus we need some variable to contain at least n x m scalars.
	# this is that variable
	tmp.x <- rep(
		x = rectangle.center.x,
		each = length(data.x)
		);

	# same as tmp.x except for y values
	tmp.y <- rep(
		x = rectangle.center.y,
		each = length(data.y)
		);

	# check if any data points are within the width of the label. Since we have a vector
	# repeated rectangle.center.x values such that the first element is repeated by the length
	# of data.x followed by the second elment and so forth. We can use vector recycling to ensure
	# that ever rectangle.center.x will be compared with every data.x
	inside.rectangle.outcome.x <- (tmp.x - (rectangle.width / 2)) < data.x & data.x < (tmp.x + (rectangle.width / 2));

	# same as inside.rectangle.outcome.x, except for y values
	inside.rectangle.outcome.y <- (tmp.y - (rectangle.height / 2)) < data.y & data.y < (tmp.y + (rectangle.height / 2));

	# generate a matrix of booleans outlining where data falls inside the any of the defined
	# rectangles.
	inside.rectangle.outcome <- matrix(
		data = inside.rectangle.outcome.x & inside.rectangle.outcome.y,
		ncol = length(data.x),
		byrow = TRUE
		);

	return(inside.rectangle.outcome);
	}

### FUNCTION TO CREATE SCATTERPLOTS ###############################################################
create.lollipopplot <- create.scatterplot <- function(
	formula, data, filename = NULL, groups = NULL, main = NULL, main.just = 'center', main.x = 0.5,
	main.y = 0.5, main.cex = 3, xlab.label = tail(sub('~', '', formula[-2]), 1),
	ylab.label = tail(sub('~', '', formula[-3]), 1), xlab.cex = 2, ylab.cex = 2, xlab.col = 'black',
	ylab.col = 'black', xlab.top.label = NULL, xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = 'center',
	xlab.top.x = 0.5, xlab.top.y = 0, xlimits = NULL, ylimits = NULL, xat = TRUE, yat = TRUE, xaxis.lab = NA,
	yaxis.lab = NA, xaxis.log = FALSE, yaxis.log = FALSE, xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.rot = 0,
	yaxis.rot = 0, xaxis.fontface = 'bold', yaxis.fontface = 'bold', xaxis.col = 'black', yaxis.col = 'black',
	xaxis.tck = c(1, 1), yaxis.tck = c(1, 1), add.grid = FALSE, xgrid.at = xat, ygrid.at = yat, grid.colour = NULL,
	horizontal = FALSE, type = 'p', cex = 0.75, pch = 19, col = 'black', col.border = 'black', lwd = 1, lty = 1,
	alpha = 1, axes.lwd = 1, strip.col = 'white', strip.cex = 1, strip.fontface = 'bold', y.error.up = NULL,
	y.error.down = y.error.up, x.error.right = NULL, x.error.left = x.error.right, y.error.bar.col = 'black',
	x.error.bar.col = y.error.bar.col, error.whisker.angle = 90, error.bar.lwd = 1, error.bar.length = 0.1,
	key = list(text = list(lab = c(''))), legend = NULL, top.padding = 0.1, bottom.padding = 0.7,
	right.padding = 0.1, left.padding = 0.5, key.top = 0.1, key.left.padding = 0, ylab.axis.padding = 1,
	axis.key.padding = 1, layout = NULL, as.table = FALSE, x.spacing = 0, y.spacing = 0, x.relation = 'same',
	y.relation = 'same', add.axes = FALSE, axes.lty = 'dashed', add.xyline = FALSE, xyline.col = 'black',
	xyline.lwd = 1, xyline.lty = 1, abline.h = NULL, abline.v = NULL, abline.col = 'black', abline.lwd = 1,
	abline.lty = 1, add.curves = FALSE, curves.exprs = NULL, curves.from = min(data, na.rm = TRUE),
	curves.to = max(data, na.rm = TRUE), curves.col = 'black', curves.lwd = 2, curves.lty = 1, add.rectangle = FALSE,
	xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL,
	col.rectangle = 'transparent', alpha.rectangle = 1, add.points = FALSE, points.x = NULL, points.y = NULL,
	points.pch = 19, points.col = 'black', points.col.border = 'black', points.cex = 1, add.line.segments = FALSE,
	line.start = NULL, line.end = NULL, line.col = 'black', line.lwd = 1, add.text = FALSE, text.labels = NULL,
	text.x = NULL, text.y = NULL, text.col = 'black', text.cex = 1, text.fontface = 'bold', text.guess.labels = FALSE,
	text.guess.skip.labels = TRUE, text.guess.ignore.radius = FALSE, text.guess.ignore.rectangle = FALSE,
	text.guess.radius.factor = 1, text.guess.buffer.factor = 1, text.guess.label.position = NULL, height = 6,
	width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE,
	description = 'Created with BoutrosLab.plotting.general', style = 'BoutrosLab', preload.default = 'custom',
	group.specific.colouring = TRUE, use.legacy.settings = FALSE, inside.legend.auto = FALSE, 
	regions.labels = c(), regions.start = c(), regions.stop = c(), regions.color = c('red'), regions.cex = 1,
	regions.alpha = 1, lollipop.bar.y = NULL, lollipop.bar.color = 'gray',  ...
	) {

	### store data on mount
	tryCatch({
			dir.name <- '/.mounts/labs/boutroslab/private/BPGRecords/Objects';
			if ( !dir.exists(dir.name) ) {
				dir.create(dir.name);
				}			
			funcname <- 'create.scatterplot';
			print.to.file(dir.name, funcname, data, filename);
			},
		warning = function(w) {
			},
		error = function(e) {
			}
		);

	function.name <- match.call()[[1]];

	lollipop.plot <- FALSE;
	if (function.name == 'create.lollipopplot') {
		lollipop.plot <- TRUE;
		}

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

	if (!is.null(yat) && length(yat) == 1) {
		if (yat == 'auto') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]));
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
			}

		else if (yat == 'auto.linear') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]), log.scaled = FALSE);
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
			}

		else if (yat == 'auto.log') {
			out <- auto.axis(unlist(data[toString(formula[[2]])]), log.scaled = TRUE);
			data[toString(formula[[2]])] <- out$x;
			yat <- out$at;
			yaxis.lab <- out$axis.lab;
			}
		}

	if (!is.null(xat) && length(xat) == 1) {
		if (xat == 'auto') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]));
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		else if (xat == 'auto.linear') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = FALSE);
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		else if (xat == 'auto.log') {
			out <- auto.axis(unlist(data[toString(formula[[3]])]), log.scaled = TRUE);
			data[toString(formula[[3]])] <- out$x;
			xat <- out$at;
			xaxis.lab <- out$axis.lab;
			}
		}

	# add preloaded defaults
	if (preload.default == 'paper') {
		}
	else if (preload.default == 'web') {
		}

	# update groups function
	groups.new <- eval(substitute(groups), data, parent.frame());

	# error checking
	if (add.curves & !all(sapply(data, is.numeric))) {
		warning('Curves cannot be drawn with grouping variables.');
		data <- data[, sapply(data, is.numeric)];
		}

	# auto set parameters
	if (length(xat) == 1 && xat == TRUE && length(xlimits) == 0) {

		adjustedformula <- as.formula(paste0(as.character(formula[2]), '~', strsplit(as.character(formula[3]), ' [|] ')[[1]][1]));

		if (is.numeric(model.frame(adjustedformula, data)[1, 2])) {
			minimum <- min(model.frame(adjustedformula, data)[2]);
			maximum <- max(model.frame(adjustedformula, data)[2]);

			# if minimum is greater than 0 make sure to display 0
			minimum <- min(minimum, 0);
			difference <- maximum - minimum;
			lognumber <- floor(log(difference, 10));

			# depending on difference, the labels will be multiples of 5, 10 or 20
			if (difference < (10 ** lognumber * 4)) { factor <- (10 ** lognumber) / 2; }
			else if (difference < (10 ** lognumber * 7)) { factor <- (10 ** lognumber); }
			else { factor <- (10 ** lognumber) * 2; }

			addition <- factor / 2;

			# depending on minimum create a sequence of at locations with padding
			if (minimum == 0) { at <- seq(0, factor * round(maximum / factor) + addition, factor); }
			else {
				at <- seq(factor * round(minimum / factor), factor * round(maximum / factor) + addition, factor);
				# only add padding to minimum if it is not 0
				minimum <- minimum - addition;
				}

			# add padding to max
			maximum <- maximum + addition;
			xlimits <- c(minimum, maximum);
			xat <- at;
			}
		}

	if (length(yat) == 1 && yat == TRUE && length(ylimits) == 0) {

		adjustedformula <- as.formula(paste0(as.character(formula[2]), '~', strsplit(as.character(formula[3]), ' [|] ')[[1]][1]));

		if (is.numeric(as.vector(model.frame(adjustedformula, data)[1, 1]))) {
			minimum <- min(model.frame(adjustedformula, data)[1]);
			maximum <- max(model.frame(adjustedformula, data)[1]);

			# if minimum is greater than 0 make sure to display 0
			minimum <- min(minimum, 0);

			# special case, if all y are the same value, and that value is 0
			if ((minimum == 0) & (maximum == 0)) {
				minimum <- -1;
				maximum <- 1;
				}

			difference <- maximum - minimum;
			lognumber <- floor(log(difference, 10));

			# special case, if all y are the same value, and that value is < 0
			if ((difference == 0) & (minimum < 0)) {
				maximum <- max(maximum, 0);
				difference <- maximum - minimum;
				lognumber <- floor(log(difference, 10));
				}

			# depending on difference, the labels will be multiples of 5,10 or 20
			if (difference < (10 ** lognumber * 4)) { factor <- (10 ** lognumber) / 2; }
			else if (difference < (10 ** lognumber * 7)) { factor <- (10 ** lognumber); }
			else { factor <- (10 ** lognumber) * 2; }

			addition <- factor / 2;

			# depending on minimum create a sequence of at locations with padding
			if (minimum == 0) {
				at <- seq(0, factor * round(maximum / factor) + addition, factor);
				}
			else {
				at <- seq(factor * round(minimum / factor), factor * round(maximum / factor) + addition, factor);
				# only add padding to minimum if it is not 0
				minimum <- minimum - addition;
				}

			# add padding to max
			maximum <- maximum + addition;
			ylimits <- c(minimum, maximum);
			yat <- at;
			}
		}

	# If formula contains conditioning variables, then only one colour is allowed for
	# horizontal error bars, and one colour is allowed for vertical error bars.
	# In other words, under conditioning, the error bar colouring currently does NOT
	# respect grouping. This issue needs to be resolved in the future.
	if ('|' %in% all.names(formula)) {
		if (length(x.error.bar.col) > 1) {
			stop(
				paste(
					'When there are conditioning variables, only one horizontal error bar colour is currently supported, but length of given x.error.bar.col is ',
					length(x.error.bar.col),
					'.',
					sep = ''
					)
				);
			}

		if (length(y.error.bar.col) > 1) {
			stop(
				paste(
					'When there are conditioning variables, only one vertical error bar colour is currently supported, but length of given y.error.bar.col is ',
					length(y.error.bar.col),
					'.',
					sep = ''
					)
				);
			}
		}

	# If formula does NOT contain conditioning variables,
	# then group-specific error bar colouring is supported.
	else if (group.specific.colouring == TRUE) {
		# generate the vector of colours for horizontal error bars, by remapping
		# the levels of the grouping variable according to x.error.bar.col
		# if x.error.bar.col is NOT a vector, do nothing
		if (length(x.error.bar.col) > 1) {
			if (length(x.error.bar.col) == length(levels(groups))) {
				x.error.bar.col <- as.character(factor(groups, labels = x.error.bar.col));
				}
			else {
				stop(
					paste(
						'The number (',
						length(levels(groups)),
						') of levels of the grouping variable does not equal the length (',
						length(x.error.bar.col),
						') of x.error.bar.col.',
						' set group.specific.colouring = FALSE to acoomplish this',
						sep = ''
						)
					);
				}
			}

		# generate the vector of colours for vertical error bars, by remapping
		# the levels of the grouping variable according to y.error.bar.col
		# if y.error.bar.col is NOT a vector, do nothing
		if (length(y.error.bar.col) > 1) {
			if (length(y.error.bar.col) == length(levels(groups))) {
				y.error.bar.col <- as.character(factor(groups, labels = y.error.bar.col));
				}
			else {
				stop(
					paste(
						'The number (',
						length(levels(groups)),
						') of levels of the grouping variable does not equal the length (',
						length(y.error.bar.col),
						') of y.error.bar.col.',
						' set group.specific.colouring = FALSE to acoomplish this',
						sep = ''
						)
					);
				}
			}
		}

	### AUTOMATIC LABEL POSITIONING ################################################################
	# Variables to decide whether or not the program should continue with the automatic
	# labeling algorithm. This variables purpose is to help organize the code and prevent
	# a large amount of indentation
	safe.to.guess <- TRUE;
	added.x <- vector();
	added.y <- vector();
	if (add.text) {

		### VARIABLE DECLARATION ###############################################################
		# These variables are used to define only those values which are used in the automatic label
		# positioning algorithm
		guess.x <- vector();
		guess.y <- vector();
		guess.labels <- vector();
		guess.col <- vector();
		guess.cex <- vector();
		guess.fontface <- vector();
		guess.skip.labels <- vector();
		guess.radius.factor <- vector();
		guess.buffer.factor <- vector();
		guess.label.position <- vector();
		guess.ignore.radius <- vector();
		guess.ignore.rectangle <- vector();

		### PARAMETER CHECKS ###################################################################
		# in order to automatically guess label positions, the user must have defined at least
		# one value in text.guess.labels to be TRUE.
		if (!any(text.guess.labels)) {
			# No label positions will be guessed
			safe.to.guess <- FALSE;
			}

		# The block within this if statement has organized parameter checks checking if the data types
		# of the input variables are correct. If a single variable or multiple variables are incorrect
		# a warning will be thrown per incorrect variable and safe.to.guess will become FALSE preventing
		# any further paramter blocks from occuring and the overall automatic label positioning algorithm
		if (safe.to.guess) {

			# Ensure text.x is a numeric
			if (!is.numeric(text.x)) {
				warning("Argument 'text.x' is not numeric, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.y is a numeric
			if (!is.numeric(text.y)) {
				warning("Argument 'text.y' is not numeric, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.guess.radius.factor is a numeric
			if (!is.numeric(text.guess.radius.factor)) {
				warning("Argument 'text.guess.radius.factor' is not numeric, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.guess.buffer.factor is a numeric
			if (!is.numeric(text.guess.buffer.factor)) {
				warning("Argument 'text.guess.buffer.factor' is not numeric, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.guess.label.position is a integer
			if (!is.numeric(text.guess.label.position) & !is.null(text.guess.label.position)) {
				warning("Argument 'text.guess.label.position' is not numeric, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.guess.labels is logical
			if (!is.logical(text.guess.labels)) {
				warning("Argument 'text.guess.labels' is not logical, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.skip.labels is logical
			if (!is.logical(text.guess.skip.labels)) {
				warning("Argument 'text.guess.skip.labels' is not logical, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.ignore.radius is logical
			if (!is.logical(text.guess.ignore.radius)) {
				warning("Argument 'text.guess.ignore.radius' is not logical, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}

			# Ensure text.guess.ignore.rectangle is logical
			if (!is.logical(text.guess.ignore.rectangle)) {
				warning("Argument 'text.guess.ignore.rectangle' is not logical, automatic label positioning will not occur.");
				safe.to.guess <- FALSE;
				}
			}

		# The block within this if statement has organized parameter checks checking if all variables
		# are multiples of the largest fundamental variable. Fundamental variables are text.guess.labels,
		# text.x or text.y. If a single variable or multiple variables are not multiples of the max length
		# of these three variables a warning will be thrown per incorrect variable and safe.to.guess will
		# become FALSE preventing any further paramter blocks from occuring and the overall automatic
		# label positioning algorithm.
		if (safe.to.guess) {

			# Calculate the max length of the variables. This is used to check if all
			# other variables are multiples of this length.
			len <- max(length(text.guess.labels), length(text.x), length(text.x));

			# Check if text.x is a multiple of text.guess.labels or text.y
			if (len %% length(text.x) != 0) {
				warning("Argument 'text.x' is not a multiple of 'text.guess.labels' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.y is a multiple of text.guess.labels or text.x
			if (len %% length(text.y) != 0) {
				warning("Argument 'text.y' is not a multiple of 'text.guess.labels' or 'text.x'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.labels is a multiple of text.x or text.y
			if (len %% length(text.guess.labels) != 0) {
				warning("Argument 'text.guess.labels' is not a multiple of 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.radius.factor is a multiple of text.guess.labels, text.x or text.y
			if (len %% length(text.guess.radius.factor) != 0) {
				warning("Argument 'text.guess.radius.factor' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.buffer.factor is a multiple of text.guess.labels, text.x or text.y
			if (len %% length(text.guess.buffer.factor) != 0) {
				warning("Argument 'text.guess.buffer.factor' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.label.position is a multiple of text.guess.labels, text.x or text.y
			if (!is.null(text.guess.label.position)) {
				if (len %% length(text.guess.label.position) != 0) {
					warning("Argument 'text.guess.label.position' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
					safe.to.guess <- FALSE;
					}
				}

			# Check if text.guess.skip.labels is a multiple of text.guess.labels, text.x or text.y
			if (len %% length(text.guess.skip.labels) != 0) {
				warning("Argument 'text.guess.skip.labels' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.ignore.radius is a multiple of text.guess.labels, text.x or text.y
			if (len %% length(text.guess.ignore.radius) != 0) {
				warning("Argument 'text.guess.ignore.radius' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}

			# Check if text.guess.ignore.rectangle is a multiple of text.guess.labels, text.x or text.y
			if (len %% length(text.guess.ignore.rectangle) != 0) {
				warning("Argument 'test.guess.ignore.rectangle' is not a multiple of 'text.guess.labels', 'text.x' or 'text.y'.");
				safe.to.guess <- FALSE;
				}
			}

		# This is the final if block for safe.to.guess. If all other paramter checks have passed
		# The algorithm will continue.
		if (safe.to.guess) {

			# The following code makes all variables the same length so that variables can be
			# divided into two groups. The first group being the set of variables responsible
			# for manual labeling positioning. The second gorup being the set of variables
			# responsible for automatic labelling.
			len <- max(length(text.guess.labels), length(text.x), length(text.x));

			text.x <- rep(
				x = text.x,
				times = len / length(text.x)
				);

			text.y <- rep(
				x = text.y,
				times = len / length(text.y)
				);

			text.col <- rep(
				x = text.col,
				times = len / length(text.col)
				);

			text.cex <- rep(
				x = text.cex,
				times = len / length(text.cex)
				);

			text.fontface <- rep(
				x = text.fontface,
				times = len / length(text.fontface)
				);

			text.guess.skip.labels <- rep(
				x = text.guess.skip.labels,
				times = len / length(text.guess.skip.labels)
				);

			text.guess.labels <- rep(
				x = text.guess.labels,
				times = len / length(text.guess.labels)
				);

			text.guess.ignore.radius <- rep(
				x = text.guess.ignore.radius,
				times = len / length(text.guess.ignore.radius)
				);

			text.guess.ignore.rectangle <- rep(
				x = text.guess.ignore.rectangle,
				times = len / length(text.guess.ignore.rectangle)
				);

			text.guess.radius.factor <- rep(
				x = text.guess.radius.factor,
				times = len / length(text.guess.radius.factor)
				);

			text.guess.buffer.factor <- rep(
				x = text.guess.buffer.factor,
				times = len / length(text.guess.buffer.factor)
				);

			if (!is.null(text.guess.label.position)) {
				text.guess.label.position <- rep(
					x = text.guess.label.position,
					times = len / length(text.guess.label.position)
					);
				}

			# These are the variables responsible for the automatic labeling
			# separate the values from the mixed paramters into the only
			# automatic labeling variables
			guess.x		<- text.x[text.guess.labels];
			guess.y		<- text.y[text.guess.labels];
			guess.labels	<- text.labels[text.guess.labels];
			guess.col	<- text.col[text.guess.labels];
			guess.cex	<- text.cex[text.guess.labels];
			guess.fontface	<- text.fontface[text.guess.labels];
			guess.skip.labels	<- text.guess.skip.labels[text.guess.labels];
			guess.radius.factor	<- text.guess.radius.factor[text.guess.labels];
			guess.buffer.factor	<- text.guess.buffer.factor[text.guess.labels];

			if (!is.null(text.guess.label.position)) {
				guess.label.position <- text.guess.label.position[text.guess.labels];
				}

			guess.ignore.radius <- text.guess.ignore.radius[text.guess.labels];
			guess.ignore.rectangle <- text.guess.ignore.rectangle[text.guess.labels];

			# These are the variables responsible for the manual labeling
			# remove all the automatic labeling values as they have already
			# been stored in separate variables.
			text.x		<- text.x[!text.guess.labels];
			text.y		<- text.y[!text.guess.labels];
			text.labels	<- text.labels[!text.guess.labels];
			text.col	<- text.col[!text.guess.labels];
			text.cex	<- text.cex[!text.guess.labels];
			text.fontface	<- text.fontface[!text.guess.labels];

			# This is the variable which will store the final guessed values from the
			# automatic labeling algorithm for the x coordinate. It corresponds to the
			# x position for the center of the label (rectangle).
			final.x <- vector(
				mode = 'numeric',
				length = length(guess.x)
				);

			# The same as final.x except for the y coordinate
			final.y <- vector(
				mode = 'numeric',
				length = length(guess.x)
				);

			# obtain x and y values from data to use in calculations
			adjustedformula <- as.formula(paste0(as.character(formula[2]), '~', strsplit(as.character(formula[3]), ' [|] ')[[1]][1]));
			data.x <- as.numeric(as.matrix(model.frame(adjustedformula, data)[2]));
			data.y <- as.numeric(as.matrix(model.frame(adjustedformula, data)[1]));

			# boolean used to check if xlimits or ylimits are defined
			# There are two options at defining the data space for a trellis object. One can
			# poorly estimate using the range of the data or they can have the best estimate by
			# using defined xlimits and ylimits of a graph. In order to produce accurate label positions
			# it is recommended that xlimits and ylimits be defined
			better.guess <- TRUE;

			# check if xlimits is defined to obtain range for radius and text width calculations
			if (is.null(xlimits)) {
				warning("Argument 'xlimits' is undefined, collision of text and points is more likely.");
				better.guess <- FALSE;
				}

			# check is ylimits is defined to obtain range for radius and text width calculations
			if (is.null(ylimits)) {
				warning("Argument 'ylimits' is undefined, collision of text and points is more likely.");
				better.guess <- FALSE;
				}

			# try and determine the plot range to calculate the most accurate radius
			range.x <- NA;
			range.y <- NA;

			# vectors storing the added guessed positions. This is different from final.x and
			# final.y as they will not store NA's if variables are skipped
			added.x <- vector();
			added.y <- vector();

			# if xlimits and ylimits are defined, use these for range calculations
			if (better.guess) {
				range.x <- xlimits[2] - xlimits[1];
				range.y <- ylimits[2] - ylimits[1];
				}
			else {
				# otherwise, guess that the graph limits will be close to the extremities
				tmp.x <- range(data.x);
				range.x <- tmp.x[2] - tmp.x[1];
				tmp.y <- range(data.y);
				range.y <- tmp.y[2] - tmp.y[1];
				}

			# vectors storing the added guessed positions. This is different from final.x and
			# final.y as they will not store NA's if variables are skipped

			for (i in 1:length(guess.x)) {

				# calculates the text width of each label.
				# used to check if points are in label position areas
				unit.text.width <- strwidth(
					s = guess.labels[i],
					units = 'figure',
					cex = guess.cex[i]
					);

				# calculates the text height of each label.
				# used to check if points are in label position areas
				unit.text.height <- strheight(
					s = guess.labels[i],
					units = 'figure',
					cex = guess.cex[i]
					);

				# calculate the width of some character string.
				# This string has been predetermined to produce relatively good
				# radius lengths
				unit.radius <- strwidth(
					s = '----',
					units = 'figure',
					cex = guess.cex[i]
					);

				unit.buffer <- unit.radius;

				# if radius.factor is defined, adjust unit.radius by the factor
				unit.radius <- unit.radius * guess.radius.factor[i];

				# if radius.factor is defined, adjust unit.radius by the factor
				unit.buffer <- unit.buffer * guess.buffer.factor[i];

				# produce 360 potential points around each (guess.x, guess.y)
				angles <- 0:359 * pi / 180;

				# Calculate the x axis radius of the ellipse for
				radius.x <- guess.x[i] + (unit.radius * range.y * cos(angles));
				buffer.x <- guess.x[i] + ( (unit.radius + unit.buffer) * range.x * cos(angles));

				radius.y <- guess.y[i] + (unit.radius * range.y * sin(angles));
				buffer.y <- guess.y[i] + ( (unit.radius + unit.buffer) * range.y * sin(angles));

				# Determine the center positions of the text boxes
				rect.x <- (guess.x[i] + ( (unit.text.width / 2) + unit.radius) * range.x * cos(angles));
				rect.y <- (guess.y[i] + ( (unit.text.height / 2) + unit.radius) * range.y * sin(angles));

				if (is.numeric(guess.label.position[i])) {
					rect.x <- (guess.x[i] + ( (unit.text.width / 2) + unit.radius) * range.x * cos(guess.label.position[i] * pi / 180));
					rect.y <- (guess.y[i] + ( (unit.text.height / 2) + unit.radius) * range.y * sin(guess.label.position[i] * pi / 180));
					final.x[i] <- rect.x;
					final.y[i] <- rect.y;
					added.x <- c(added.x, final.x[i]);
					added.y <- c(added.y, final.y[i]);
					}

				else {
					# We remove the data point which is identical to guess.x and guess.y so that the algorithm
					# doesn't mistakenly consider this as a point within the ellipse, which will result in removed
					# label positions.
					valid.data.points <- !(data.x == guess.x[i] & data.y == guess.y[i]);

					# This is a boolean for each potential label position around a single guess.x and guess.y.
					# During the algorithm, positions around an ellipse are determined to be suitable or not
					# suitable for label positioning. If they are sutiable they remain true, otherwise, they
					# are false and are no longer available for labelling.
					valid.labels <- rep(TRUE, 360);

					# Enter the ellipse calculation portion of the algorithm
					if (!guess.ignore.radius[i]) {

						in.ellipse <- as.vector(inside.ellipse(
								ellipse.center.x = guess.x[i],
								ellipse.center.y = guess.y[i],
								ellipse.radii.x = (unit.radius + unit.buffer) * range.x,
								ellipse.radii.y = (unit.radius + unit.buffer) * range.y,
								data.x = data.x[valid.data.points],
								data.y = data.y[valid.data.points]
								));

						if (any(in.ellipse)) {

							for (j in order(in.ellipse, decreasing = TRUE)[1:sum(in.ellipse)]) {

								tmp.x <- data.x[valid.data.points][j];
								tmp.y <- data.y[valid.data.points][j];

								line.slope <- (tmp.y - guess.y[i]) / (tmp.x - guess.x[i]);
								perp.line.slope <- -1 / line.slope;
								vertical.shift <- guess.y[i] - (perp.line.slope * guess.x[i]);

								other.x <- tmp.x + 1;
								other.y <- (perp.line.slope * other.x) + vertical.shift;

								if (!is.finite(perp.line.slope)) {
									other.y <- guess.y + 1;
									other.x <- guess.x;
									}

								# INFORMATION PLEASE
								position <- sign(
									(other.x - guess.x[i]) * (buffer.y - guess.y[i]) -
									(other.y - guess.y[i]) * (buffer.x - guess.x[i])
									);

								# COMMENTS
								if (!is.finite(perp.line.slope)) {
									if (tmp.x >= guess.x[i]) {
										valid.labels <- valid.labels & (position < 1);
										}
									else {
										valid.labels <- valid.labels & (position > -1);
										}
									}

								else if (tmp.x >= (tmp.y - vertical.shift) / perp.line.slope) {
									valid.labels <- valid.labels & (position < 1);
									}

								else {
									valid.labels <- valid.labels & (position > -1);
									}
								}
							}
						}

					# COMMENT
					if (!guess.ignore.rectangle[i]) {

						in.rectangle <- inside.rectangle(
							rectangle.center.x = rect.x[valid.labels],
							rectangle.center.y = rect.y[valid.labels],
							rectangle.width = unit.text.width * range.x,
							rectangle.height = unit.text.height * range.y,
							data.x = c(data.x[valid.data.points], added.x),
							data.y = c(data.y[valid.data.points], added.y)
							);

						valid.labels[valid.labels] <- !(apply(in.rectangle, 1, any));
						}

					middle.x <- mean(data.x);
					middle.y <- mean(data.y);

					if (any(valid.labels) || !guess.skip.labels[i]) {

						if (!any(valid.labels) && !guess.skip.labels[i]) {
							valid.labels <- rep(TRUE, 360);
							}

						# COMMENT lapply instead of this stuff?
						longest.arcs <- 0;
						current.arcs <- 0;
						indexes <- vector();

						if (!all(valid.labels)) {
							valid.labels.true.pos <- which(valid.labels);
							if (tail(valid.labels.true.pos, n = 1) == length(valid.labels)) {
								tail.arc <- vector();
								last <- length(valid.labels) + 1;
								index <- 0;
								for (j in length(valid.labels.true.pos):1) {
									if (last == valid.labels.true.pos[j] + 1) {
										tail.arc <- c(tail.arc, index);
										index <- index - 1;
										last <- valid.labels.true.pos[j];
										}
									else {
										break;
										}
									}
								valid.labels.true.pos <- c(
									rev(tail.arc),
									valid.labels.true.pos[1:(length(valid.labels.true.pos) - length(tail.arc))]
									);
								}

							arcs <- 0;
							prev <- valid.labels.true.pos[1] - 1;
							index <- 1;
							arc.index <- vector();

							for (j in 1:length(valid.labels.true.pos)) {
								if (valid.labels.true.pos[j] == prev + 1) {
									arcs[index] <- arcs[index] + 1;
									if (j == length(valid.labels.true.pos)) {
										arc.index <- c(arc.index, valid.labels.true.pos[j - arcs[index] / 2]);
										}
									}
								else {
									arc.index <- c(arc.index, valid.labels.true.pos[j - arcs[index] / 2]);
									arcs <- c(arcs, 1);
									index <- index + 1;
									}
								prev <- valid.labels.true.pos[j];
								}
							longest.arc <- max(arcs);
							selected.arcs <- arc.index[arcs == longest.arc];

							if (sum(selected.arcs) > 1) {
								distance <- sqrt( (rect.x[selected.arcs] - middle.x) ^ 2 + (rect.y[selected.arcs] - middle.y) ^ 2);
								selected.arcs <- selected.arcs[order(distance, decreasing = TRUE)][1];
								}

							### SELECT THE POINTS TO LABEL #########################################
							final.x[i] <- rect.x[selected.arcs];
							final.y[i] <- rect.y[selected.arcs];
							added.x <- c(added.x, final.x[i]);
							added.y <- c(added.y, final.y[i]);
							}
						else {
							### any all ... confusing
							if (!any(valid.labels)) {
								final.x[i] <- NA;
								final.y[i] <- NA;
								}
							else {
								distance <- sqrt( (rect.x - middle.x) ^ 2 + (rect.y - middle.y) ^ 2);
								selected.arcs <- order(distance, decreasing = TRUE)[1];
								final.x[i] <- rect.x[selected.arcs];
								final.y[i] <- rect.y[selected.arcs];
								added.x <- c(added.x, final.x[i]);
								added.y <- c(added.y, final.y[i]);
								}
							}
						}
					}
				}
			}
		}

	# create a scatterplot and save it as a trellis object
	trellis.object <- lattice::xyplot(
		formula,
		data,
		panel = function(
			x,
			y,
			y.error.up.local = y.error.up,
			y.error.down.local = y.error.down,
			groups.local = groups.new,
			subscripts,
			type.local = type,
			abline.local = abline,
			...
			) {

			# add background rectangle if requested
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
			if (lollipop.plot) {

				# min and max values for lollipop plot
				max.x <- if (is.null(xlimits)) {max(x) + (max(x) - min(x)) * 0.07} else { xlimits[2] };
				min.x <- if (is.null(xlimits)) {min(x) - (max(x) - min(x)) * 0.07} else { xlimits[1] };
				max.y <- if (is.null(ylimits)) {max(y) + (max(y) - min(y)) * 0.07} else { ylimits[2] };
				min.y <- if (is.null(ylimits)) {min(y) - (max(y) - min(y)) * 0.07} else { ylimits[1] };

				bar.y.top <- if (is.null(lollipop.bar.y)) {min.y + (max.y - min.y) * 0.06 } else { lollipop.bar.y + (max.y - min.y) * 0.06 };
				bar.y.bottom <- if (is.null(lollipop.bar.y)) {min.y + (max.y - min.y) * 0.01 } else { lollipop.bar.y + (max.y - min.y) * 0.01 };

				region.y.top <- if (is.null(lollipop.bar.y)) {min.y + (max.y - min.y) * 0.065 } else { lollipop.bar.y + (max.y - min.y) * 0.065 };
				region.y.bottom <- if (is.null(lollipop.bar.y)) {min.y + (max.y - min.y) * 0.005 } else { lollipop.bar.y + (max.y - min.y) * 0.005 };

				bar.x.left <- min.x + (max.x - min.x) * 0.01;
				bar.x.right <- min.x + (max.x - min.x) * 0.99;
				for (i in c(1:length(x))) {
			       		panel.xyplot(
						x = c(x[i], x[i]),
				       		y = c(bar.y.top, y[i]),
						type = 'l',
						col.line = 'black',
						lwd = 1.5
						);
					}

				panel.rect(
					xleft = bar.x.left,
					ybottom = bar.y.bottom,
					xright = bar.x.right,
					ytop = bar.y.top,
					col = lollipop.bar.color,
					alpha = 1,
					border = NA
					);

				if (length(regions.start) > 0 && length(regions.stop) > 0) {
					panel.rect(
						xleft = regions.start,
						ybottom = region.y.bottom,
				       		xright = regions.stop,
						ytop = region.y.top,
						col = regions.color,
						alpha = regions.alpha,
				       		border = NA
						);
					panel.text(
						x = (regions.start + regions.stop) / 2,
						y = (bar.y.top + bar.y.bottom) / 2,
						labels = regions.labels,
						col = 'black',
						cex = regions.cex,
						fontface = 'bold'
						);
					}
				}
			# if requested, add x=0, y=0 lines
			if (add.axes) {
				panel.abline(
					h = 0,
					v = 0,
					col.line = 'black',
					lty = axes.lty,
					lwd = 1.5
					);
				}

			# if requested, add y=x line
			if (add.xyline) {
				panel.abline(
					a = 0,
					b = 1,
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
							to = curves.to[i],
							col = curves.col[i],
							lwd = curves.lwd[i],
							lty = curves.lty[i]
							)
						);
					}
				}

			# if grid-lines are requested, over-ride default behaviour
			if ('g' %in% type || add.grid == TRUE) {

				# create the grid-lines
				panel.abline(
					v = BoutrosLab.plotting.general::generate.at.final(
						at.input = xgrid.at,
						limits = xlimits,
						data.vector = data$x
						),
					h = BoutrosLab.plotting.general::generate.at.final(
						at.input = ygrid.at,
						limits = ylimits,
						data.vector = data$y
						),
					col = if (!is.null(grid.colour)) { grid.colour; } else { trellis.par.get('reference.line')$col }
					);
				}

			# create the main plot
			panel.xyplot(
				x,
				y,
				groups = groups.local,
				subscripts = subscripts,
				type = setdiff(type.local, 'g'),
				alpha = alpha,
				horizontal = horizontal,
				...
				);

			# if requested, add y-axis (i.e. vertical) error bars
			if (!is.null(y.error.up.local)) {
				panel.arrows(
					x, y + y.error.up.local[subscripts], x, y - y.error.down.local[subscripts],
					groups = groups.local,
					length = error.bar.length,
					angle = error.whisker.angle,
					ends = 'both',
					col = y.error.bar.col,
					lwd = error.bar.lwd
					);
				}

			# if requested, add x-axis (i.e. horizontal) error bars
			if (!is.null(x.error.right)) {
				panel.arrows(
					x + x.error.right[subscripts], y, x - x.error.left[subscripts], y,
					groups = groups.local,
					length = error.bar.length,
					angle = error.whisker.angle,
					ends = 'both',
					col = x.error.bar.col,
					lwd = error.bar.lwd
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

			# if requested, add user-defined vertical line
			if (!is.null(abline.v)) {
				panel.abline(
					v   = abline.v,
					lty = abline.lty,
					lwd = abline.lwd,
					col = abline.col
					);
				}

			# if requested, add additional lines
			if (add.line.segments) {

				for (i in 1:length(line.start)) {
					with(
						data = new.env(),
						expr = panel.segments(
							x, line.start[[i]][subscripts], x, line.end[[i]][subscripts],
							col = line.col[[i]],
							lwd = line.lwd[[i]],
							lineend = 1
							)
						);
					}
				}

			# if requested, add additional points
			if (add.points) {
				panel.points(
					x = points.x,
					y = points.y,
					pch = points.pch,
					col = mapply(
						function(pch, spot.colours, spot.border) {
							if (pch %in% 0:20) { return(spot.colours); } else if (pch %in% 21:25) { return(spot.border); }
							},
						points.pch,
						spot.colours = points.col,
						spot.border = points.col.border
						),
					fill = mapply(
						function(pch, spot.colours) {
							if (pch %in% 0:20) { NA; } else if (pch %in% 21:25) { return(spot.colours); }
							},
						points.pch,
						spot.colours = points.col
						),
					cex = points.cex
					);
				}

			# if requested, add point labels
			if (any(!text.guess.labels) && add.text) {
				panel.text(
					x = text.info$x,
					y = text.info$y,
					labels = text.info$labels,
					col = text.info$col,
					cex = text.info$cex,
					fontface = text.info$fontface
					);
				}

			if (any(text.guess.labels) && add.text && safe.to.guess) {
				panel.text(
					x = final.x,
					y = final.y,
					labels = guess.labels,
					col = guess.col,
					cex = guess.cex,
					fontface = guess.fontface
					);
				}
			},
		type = type,
		cex = cex,
		pch = pch,
		col = mapply(
			function(point.pch, point.colours, point.border) {
				if (point.pch %in% 0:20) { return(point.colours); } else
				if (point.pch %in% 21:25) { return(point.border); } else
				if (! point.pch %in% 0:25) { return(point.colours); }
				},
			point.pch = pch,
			point.colours = col,
			point.border = col.border
			),
		fill = mapply(
			function(point.pch, point.colours) {
				if (point.pch %in% 0:20) { NA; } else
				if (point.pch %in% 21:25) { return(point.colours); } else
				if (! point.pch %in% 0:25) { return(point.colours); }
				},
			point.pch = pch,
			point.colours = col
			),
		lwd = lwd,
		lty = lty,
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
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					cex = xaxis.cex,
					rot = xaxis.rot,
					limits = xlimits,
					fontface = if ('Nature' == style) { 'plain' } else { xaxis.fontface },
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
				use.legacy.settings = use.legacy.settings || ('Nature' == style),
				add.to.list = list(
					cex = yaxis.cex,
					rot = yaxis.rot,
					limits = ylimits,
					fontface = if ('Nature' == style) { 'plain' } else { yaxis.fontface },
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
				lwd = axes.lwd,
				col = if ('Nature' == style) { 'transparent' } else { 'black' }
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 1 },
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
			cex = strip.cex,
			fontface = strip.fontface
			),
		key = key,
		legend = legend
		);

	if (inside.legend.auto) {
		extra.parameters <- list('data' = data, 'formula' = formula, 'ylimits' = trellis.object$y.limits,
			'xlimits' = trellis.object$x.limits);
		coords <- c();
		coords <- .inside.auto.legend('create.scatterplot', filename, trellis.object, height, width, extra.parameters);
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
