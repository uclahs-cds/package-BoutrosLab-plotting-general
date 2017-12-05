# The BoutrosLab.plotting.general package is copyright (c) 2014 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO DISPLAY AVAILABLE COLOUR SCHEMES ##################################################
show.available.palettes <- function(
	type = 'general', filename = NULL, height = 5, width = 8, resolution = 300
	) {

	type <- tolower(type);

	### DISPLAY GENERAL SCHEMES ###################################################################
	# The general schemes are those found in the default.colours function
	if ('general' == type) {

		# Get the default colours
		general <- default.colours(palette.type = 'all');
		all.colours <- stack(general)$values;

		# Encoding the colours as numbers
		max.length <- 0;

		for (i in 1:length(general)) {
			if (length(general[[i]]) > max.length) {
				max.length <- length(general[[i]]);
				}
			}

		count <- 1;
		for (i in 1:length(general)) {
			for (j in 1:max.length) {
				if (!is.na(general[[i]][j])) {
					general[[i]][j] <- count;
					count <- count + 1;
					}
				}
			}

		for (i in 1:length(general)) {
			if (length(general[[i]]) < max.length) {
				length(general[[i]]) <- max.length;
				}
			}

		# Making the data frame numeric for heatmap functionality
		reordered.data <- as.data.frame(lapply(general, as.numeric));

		# Using the heatmap function to display the colour palettes
		BoutrosLab.plotting.general::create.heatmap(
			filename = filename,
			# reverse order of columns for display
			x = reordered.data[c(rev(seq(1, dim(reordered.data)[2], 1)))],
			clustering.method = 'none',
			colour.scheme = all.colours,
			total.colours = (length(all.colours) + 1),
			fill.colour = 'white',
			grid.row = TRUE,
			grid.col = TRUE,
			row.colour = 'white',
			col.colour = 'white',
			row.lwd = 10,
			axes.lwd = 0,
			print.colour.key = FALSE,
			# Reverse order of names to match reversed data frame
			yaxis.lab = rev(names(general)),
			yaxis.cex = 1,
			yaxis.fontface = 1,
			height = height,
			width = width,
			resolution = resolution
			);
		}

	### DISPLAY SPECIFIC SCHEMES ##################################################################
	else if ('specific' == type) {

		specific <- force.colour.scheme(x = '', scheme = 'all', return.scheme = TRUE)$scheme;

		# sort by length, longest first
		swapped <- TRUE;
		while (TRUE == swapped) {
			swapped <- FALSE;
			for (i in 2:length(specific)) {
				if (length(specific[[i - 1]]$colours) < length(specific[[i]]$colours)) {
					temp			<- specific[i];
					temp.name		<- names(specific)[i];
					specific[i]		<- specific[i - 1];
					names(specific)[i] 	<- names(specific)[i - 1];
					specific[i - 1]		<- temp;
					names(specific)[i - 1]	<- temp.name;

					swapped <- TRUE;
					}
				}
			}

		number.of.colours <- 0;
		for (i in 1:length(specific)) {
			number.of.colours <- number.of.colours + length(specific[[i]]$levels);
			}

		# calculate layout: (assumption: longest palette is much longer than others -- therefore sets height of plot)
		# adding one spacer for the header text
		display.height <- length(specific[[1]]$colours) + 1;

		formatted.data <- data.frame();

		# initate with first row
		temp.col <- c(0, specific[[1]]$colours);

		for (i in 2:length(specific)) {

			potential.length <- length(temp.col) + length(specific[[i - 1]]$colours) + 2;

			if (potential.length <= display.height && i < length(specific)) {
				temp.col <- c(temp.col, NA, 0, specific[[i]]$colours);
				}
			else {
				# special case for the last row
				if (length(specific) == i) {
					temp.col <- c(temp.col, NA, 0, specific[[i]]$colours);
					}

				# add previous col to data frame
				length(temp.col) <- display.height;

				# check if inital row has been added
				if (0 == ncol(formatted.data)) {
					formatted.data <- temp.col;
					}
				else {
					formatted.data <- cbind(formatted.data, temp.col);
					}

				# add spacers for labels
				spacer.col <- NA;
				length(spacer.col) <- display.height;

				spacing.for.labels <- 0;
				for (word in 1:length(temp.col)) {
					if (nchar(temp.col[2]) > spacing.for.labels) {
						spacing.for.labels <- nchar(temp.col[2]);
						}
					}

				for (k in 1:spacing.for.labels) {
					formatted.data <- cbind(formatted.data, spacer.col);
					}

				# make a new row and add 0 for header
				# using 0 instead of NA to mark headers in order to locate them later
				temp.col <- c(0, specific[[i]]$colours);
				}
			}

		# save the matrix with colour codes to find white swatches later
		colour.coded.data <- formatted.data;

		# map colours to numbers
		all.colours <- character();
		for (i in 1:length(specific)) {
			all.colours <- c(all.colours, specific[[i]]$colours);
			}

		colour.number <- 1;
		for (i in 1:length(formatted.data)) {
			if (!is.na(formatted.data[i]) && 0 != formatted.data[i]) {
				formatted.data[i] <- colour.number;
				colour.number <- colour.number + 1;
				}
			}

		enumerated.data <- as.data.frame(apply(formatted.data, c(1, 2), as.numeric));

		# get labels
		labels <- character();
		bold.text <- numeric();
		offset <- numeric();
		for (i in 1:length(specific)) {
			labels <- c(labels, names(specific)[i], specific[[i]]$levels);
			bold.text <- c(bold.text, 2, rep(1, length(specific[[i]]$levels)));
			offset <- c(offset, -0.5, rep(1, length(specific[[i]]$levels)));
			}

		label.col.positions <- which(!is.na(enumerated.data), arr.ind = TRUE)[, 2];
		label.row.positions <- which(!is.na(enumerated.data), arr.ind = TRUE)[, 1];

		# adding border around white swatches
		border.matrix <- matrix(
			nrow = nrow(colour.coded.data),
			ncol = ncol(colour.coded.data),
			data = FALSE
			);

		border.colour.matrix <- matrix(
			nrow = nrow(colour.coded.data),
			ncol = ncol(colour.coded.data),
			data = 'grey'
			);

		border.size.matrix <- matrix(
			nrow = nrow(colour.coded.data),
			ncol = ncol(colour.coded.data),
			data = 0.25
			);

		symbol.locations <- list(
			borders = list(
				list(
					x = border.matrix,
					col = border.colour.matrix,
					size = border.size.matrix
					)
				)
			);

		symbol.locations$borders[[1]]$x[which('white' == colour.coded.data, arr.ind = TRUE)] <- TRUE;

		# display the colours using the heatmap function
		create.heatmap(
			filename = filename,
			x = enumerated.data,
			same.as.matrix = TRUE,
			# adding white for the header spaces, which are labelled 0
			colour.scheme = c('white', all.colours),
			total.colours = (length(all.colours) + 2),
			fill.colour = 'white',
			clustering.method = 'none',
			axes.lwd = 0,
			print.colour.key = FALSE,
			# adding headers and labels
			col.pos = label.col.positions,
			# flipping the rows to match the colour layout
			row.pos = (display.height + 1 - label.row.positions),
			cell.text = labels,
			text.position = 4,
			text.offset = offset,
			text.col = 'black',
			text.cex = 0.75,
			text.fontface = bold.text,
			# adding borders around white swatches
			symbols = symbol.locations,
			height = height,
			width = width,
			resolution = resolution
			);
		}

	else {
		stop('Invalid value supplied to type parameter.');
		}
	}
