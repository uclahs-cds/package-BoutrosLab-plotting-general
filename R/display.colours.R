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

### FUNCTION TO DISPLAY COLOURS INCLUDING HOW THEY APPEAR IN GREYSCALE ############################
display.colours <- display.colors <- function(cols, names = cols) {

	number.of.colours <- length(cols);

	if (length(cols) != length(names)) {
		stop('The number of colours and the number of names must be equal.');
		}

	plot.new();
	par(mar = c(1, 1, 1, 1));

	# set spacing
	rect.y <- (par('usr')[4] - par('usr')[3]) / number.of.colours;
	starting.y <- par('usr')[4] - rect.y / 2;

	rect.x <- (par('usr')[2] - abs(par('usr')[1])) / 3;
	starting.x <- (par('usr')[2] - abs(par('usr')[1])) / 3;

	# Add the names of each colour
	for (i in 1:length(names)) {
		text(
			x = starting.x / 2,
			y = starting.y,
			labels = names[i]
			);
		starting.y <- starting.y - rect.y;
		}

	starting.y <- par('usr')[4];

	# draw coloured rectangles
	for (i in 1:length(cols)) {
		rect(
			xleft = starting.x * 2,
			ybottom = (starting.y - rect.y),
			xright = rect.x,
			ytop = starting.y,
			col = cols[i],
			lwd = 0
			);

		starting.y <- starting.y - rect.y;
		}

	# reset spacing variables to draw greyscale half
	starting.y <- par('usr')[4];
	# starting.x <- par('usr')[2];
	ending.x <- (par('usr')[2] - abs(par('usr')[1])) / 3;

	# draw greyscale rectangles
	grey.cols <- vector(length = length(cols));
	i <- 0;

	for (col in cols) {
		# convert R colour to rgb
		rgbcol <- col2rgb(col);

		# convert rgb colour to greyscale
		greyval <- (0.2989 * rgbcol[1, 1]) + (0.5870 * rgbcol[2, 1]) + (0.1140 * rgbcol[3, 1]);
		greyval <- greyval / 2.55;

		greyval <- round(greyval);
		i <- i + 1;
		grey.cols[i] <- paste('grey', greyval, sep = '');
		}

	for (i in 1:length(grey.cols)) {
		rect(
			xleft = starting.x * 3,
			ybottom = (starting.y - rect.y),
			xright = rect.x * 2,
			ytop = starting.y,
			col = grey.cols[i],
			lwd = 0
			);

		starting.y <- starting.y - rect.y;
		}
	}
