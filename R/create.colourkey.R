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

### FUNCTION TO CREATE COLOURKEYS #################################################################
create.colourkey <- create.colorkey <- function(
	x, scale.data = FALSE, colour.scheme = c(), total.colours = 99, colour.centering.value = 0,
	colour.alpha = 1, fill.colour = 'darkgray', at = NULL, colourkey.labels.at = NULL,
	colourkey.labels = colourkey.labels.at, colourkey.labels.cex = 1, placement = NULL
	) {

	### SUBSET DATA ###############################################################################
	# Scale the data if necessary
	if (scale.data) {
		x <- t(x);
		x <- scale(x);
		x <- t(x);
		}

	### AUTOMATIC COLOUR-KEY HANDLING #############################################################
	# work out data ranges, break point locations, and colour-number from input parameters
	if (is.null(at)) {
		min.value <- min(x - colour.centering.value, na.rm = TRUE);
		max.value <- max(x - colour.centering.value, na.rm = TRUE);
		at <- seq(from = min.value, to = max.value, length.out = total.colours);
		}
	else {
		min.value <- min(at - colour.centering.value, na.rm = TRUE);
		max.value <- max(at - colour.centering.value, na.rm = TRUE);
		max.at <- max(at);
		min.at <- min(at);

		if (max(x, na.rm = TRUE) > max.at) {
			warning(
				paste(
					'max(x) =',
					max(x, na.rm = TRUE),
					'is greater than max(at) = ',
					max.at,
					'Clipped data will be plotted'
					)
				);

			x[x > max.at] <- max(at);
			}

		if (min(x, na.rm = TRUE) < min.at) {
			warning(
				paste(
					'min(x) =',
					min(x, na.rm = TRUE),
					'is greater than min(at) = ',
					min.at,
					'Clipped data will be plotted'
					)
				);

			x[x < min.at] <- min(at);
			}

		total.colours <- max(length(at), total.colours);
		}

	# determine whether the data is one-sided or two-sided
	is.twosided <- sign(min.value) != sign(max.value);

	# colour-handling: use a default colour scheme if one was not provided
	if (0 == length(colour.scheme)) {
		if (is.twosided) {
			colour.scheme <- c('red', 'white', 'blue');
			}
		else {
			colour.scheme <- c('white', 'blue');
			}
		}

	# colour-handling: first handle legacy cases
	if (1 == length(colour.scheme)) {
		if (colour.scheme == 'RedWhiteBlue')	 { colour.scheme <- c('red', 'white', 'blue'); }
		else if (colour.scheme == 'WhiteBlack')      { colour.scheme <- c('white', 'black'); }
		else if (colour.scheme == 'BlueWhiteYellow') { colour.scheme <- c('blue', 'white', 'yellow'); }
		else { stop('Unknown colour scheme:', colour.scheme); }
		}

	# colour-handling: next cover one-sided colour schemes
	if (2 == length(colour.scheme)) {
		colour.function <- colorRamp(colour.scheme, space = 'Lab');
		my.palette <- rgb(colour.function(seq(0, 1, 1 / total.colours) ^ colour.alpha), maxColorValue = 255);
		}

	# colour-handling: then handle two-sided colour schemes
	if (3 == length(colour.scheme)) {

		# warn the user if they try to use a three-colour scheme with one-sided data
		if (!is.twosided) { warning('Using a three-colour scheme with one-sided data is not advised!'); }

		# create the colour scheme
		colour.function.low  <- colorRamp(colour.scheme[1 : 2], space = 'Lab');
		colour.function.high <- colorRamp(colour.scheme[2 : 3], space = 'Lab');

		# the number of negative colours is based on the fraction of the range that's below the center value
		# the number of positive colours is based on the number of negatives
		# leave one colour free for the center value
		neg.colours <- min.value / (max.value - min.value) * (total.colours - 1);
		neg.colours <- ceiling(abs(neg.colours));
		pos.colours <- total.colours - neg.colours - 1;

		# there is a potential for the colour allocation to go wrong when:
		#	1) we have one-sided data
		#	2) the colour-centering is at zero
		#	3) a three-colour scheme is requested
		# We try to automatically detect this case and provide a fix
		if (neg.colours < 1 | pos.colours < 1) {
			warning('Colour allocation scheme failed, moving to a default method');
			neg.colours <- round(total.colours / 2);
			pos.colours <- round(total.colours / 2);
			}

		# create the colour palette
		my.palette <- c(
			rgb( colour.function.low(seq(0, 1, 1 / neg.colours) ^ colour.alpha), maxColorValue = 255),
			 # this helps ensure that the values are centered properly
			colour.scheme[2],
			rgb( colour.function.high(seq(0, 1, 1 / pos.colours) ^ (1 / colour.alpha)), maxColorValue = 255)
			);
		}

	# allow colour-schemes with > 3 colours
	if (3 < length(colour.scheme)) {

		# only allow this behaviour with at-colour-type-handling
		if (is.null(at)) { stop('>3-colour schemes only work when at is specified'); }

		# create the colour scheme
		my.palette <- c(colour.scheme);
		}

	# colour-handling: lastly ensure that a palette was defined somehow
	if (!exists('my.palette')) {
		stop('Somehow no palette was ever defined');
		}

	# create the colour-key
	colour.key <- draw.colorkey(
		key = list(
			space = 'bottom',
			size = 1,
			width = 1,
			height = 1,
			at = at,
			col = my.palette,
			labels = list(
				cex = colourkey.labels.cex,
				at = colourkey.labels.at,
				labels = colourkey.labels,
				fontface = 'bold'
				),
			tick.number = 3
			),
		vp = placement,
		draw = TRUE
		);

	# output the object
	return(colour.key);
	}
