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

### FUNCTION TO CREATE A LEGEND GROB ###############################################################
legend.grob <- function(
	legends, label.cex = 1, title.cex = 1, title.just = 'centre', title.fontface = 'bold',
	font.family = NULL, size = 3, border = NULL, border.padding = 1, layout = c(1, length(legends)),
	between.col = 1, between.row = 1
	) {

	# NOTE: calls to 'draw.key' may open a device for drawing (even with 'draw = FALSE' set)
	# As far as I can tell, draw.key opens a new device only if no devices are open already
	# The current solution is to call dev.off() at the end of the function, but only if
	# no devices were open to start (ie. the open device was created by draw.key)
	# This prevents erroneously closing a device that the user had opened prior to calling
	# the function

	# Check if any devices are open when the function is called
	# If not, the device created by draw.key will be closed at the end of the function call
	devices.open <- FALSE;

	if (length(dev.list()) > 0) {
		devices.open <- TRUE;
		}

	legend.grob.final <- NULL;

	if (length(legends) > 0) {
		if(is.null(font.family)){
			font.family = BoutrosLab.plotting.general::get.defaults(property = 'fontfamily')
			}
		num.legends <- length(legends);

		# If user-specified layout is not the right size, use a default layout
		if (layout[1] * layout[2] < num.legends) {
			warning("User-specified legend layout is not large enough. Using default layout.");
			layout <- c(1, num.legends);
			}

		# Create a layout for the legends
		# Each legend gets an extra row for its title
		# There is also an extra empty row/column inserted between each of the legends to allow for nicer spacing
		legend.layout <- grid.layout(
			ncol = layout[1] * 2 - 1,
			nrow = layout[2] * 3 - 1,
			heights = unit(
				c(0, 0, rep(c(between.row, 0, 0), layout[2] - 1)),
				c(rep('lines', layout[2] * 3 - 1))
				),
			widths = unit(
				c(rep(c(0, between.col), layout[1] - 1), 0),
				c(rep('lines', layout[1] * 2 - 1))
				)
			);

		# Create a frame using this layout
		legend.grob.final <- frameGrob(layout = legend.layout);

		# Create each legend
		for (i in 1:num.legends) {

			legendi <- legends[[i]];
			typei <- names(legends)[i];

			if (is.null(legendi[['continuous.amount']])) {
				legendi[['continuous.amount']] <- 100;
				}

			switch(
				typei,
				legend = {
					# Figure out where this legend should go in the layout (ordered row-wise)
					legend.row <- if (i %% layout[1] == 0) { i / layout[1] } else { floor(i / layout[1]) + 1 }
					legend.col <- if (i %% layout[1] == 0) { layout[1] } else { i %% layout[1] }

					# Create the title of this legend
					if (!is.null(legendi[['title']])) {

						# Draw a grob representing the title
						title.x.coord <- 0.5;

						if (title.just == 'left') {
							title.x.coord <- 0;
							}
						else if (title.just == 'right') {
							title.x.coord <- 1;
							}

						title.grob <- textGrob(
							label = legendi[['title']],
							just = c(title.just, 'top'),
							x = title.x.coord,
							y = 1,
							gp = gpar(
								cex = title.cex,
								fontface = title.fontface,
								fontfamily = font.family
								)
							);

						# Get the height of this grob so we can add white space around it
						title.grob.height <- convertHeight(
							grobHeight(title.grob),
							unitTo = 'lines',
							valueOnly = TRUE
							);

						# Add the title to the frame
						legend.grob.final <- packGrob(
							frame = legend.grob.final,
							grob = title.grob,
							row = 3 * (legend.row - 1) + 1,
							col = 2 * (legend.col - 1) + 1,
							height = max(
								legend.grob.final$framevp$layout$heights[3 * (legend.row - 1) + 1],
								unit(title.grob.height + 0.4, 'lines')
								),
							force.height = TRUE
							);
						}

					# Create a key describing the content of the legend
					# The first column is the coloured rectangles and
					# the second column is the corresponding text labels
					if (!is.null(legendi[['continuous']]) && legendi[['continuous']] == TRUE) {

						colorRamp <- colorRampPalette(legendi[['colours']]);
						legend.key <- list(
							space = 'right',
							between = 0.5,
							rep = TRUE,
							tick.number = if (is.null(legendi[['tck.number']])) { 0 } else{ legendi[['tck.number']] },
							tck = if (is.null(legendi[['tck']])) { 0 } else { legendi[['tck']] },
							at = do.breaks(c(0, legendi[['continuous.amount']]), legendi[['continuous.amount']]),
							col = colorRamp,
							width = if (is.null(legendi[['width']])) { 2 } else { legendi[['width']] },
							labels = list(
								labels = if (is.null(legendi[['labels']])) { c('') } else{ legendi[['labels']] },
								at = if (is.null(legendi[['at']])) { NULL } else { legendi[['at']] },
								cex = if (is.null(legendi[['cex']])) { 0.8 } else { legendi[['cex']] },
								rot =  if (is.null(legendi[['labels.rot']])) { 0 } else { legendi[['labels.rot']] }
								)
							);

						# Add the legend to the frame
						legend.grob.final <- packGrob(
							frame = legend.grob.final,
							grob = draw.colorkey(
								key = legend.key,
								draw = FALSE,
								vp = viewport(
									x = if (is.null(legendi[['pos.x']])) { 0 } else { legendi[['pos.x']] },
									y = if (is.null(legendi[['pos.y']])) { 0 } else { legendi[['pos.y']] },
									angle = if (is.null(legendi[['angle']])) { 0 } else { legendi[['angle']] },
									just = if(is.null(legendi[['just']])){ c("left", "bottom") } else { legendi[['just']] }
									)
								),
							row = 3 * (legend.row - 1) + 2,
							col = 2 * (legend.col - 1) + 1,
							height = if (is.null(legendi[['height']])) { unit(1, 'lines') } else { unit(legendi[['height']],'lines') },
							);
						}
					else {
						legend.key <- list(
							just = c('left', 'top'),
							between = 0.5,
							rep = FALSE,
							rectangles = list(
								col = legendi[['colours']],
								size = if (is.null(legendi[['size']])) { size } else { legendi[['size']] },
								height = 1,
								border = legendi[['border']]
								),
							text = list(
								legendi[['labels']],
								cex = label.cex,
								fontfamily = font.family
								)
							);

						# Add the legend to the frame
						legend.grob.final <- packGrob(
							frame = legend.grob.final,
							grob = draw.key(key = legend.key, draw = FALSE),
							row = 3 * (legend.row - 1) + 2,
							col = 2 * (legend.col - 1) + 1
							);
						}
					}
				);
			}

		# Draw border if specified
		if (!is.null(border)) {

			if (!is.list(border)) {
				stop("Argument border of legend.grob must be a list.");
				}

			border[['fill']] <- 'transparent';

			# Calculate the width and height of the legend
			legend.grob.width <- convertWidth(
				grobWidth(legend.grob.final),
				unitTo = 'lines',
				valueOnly = TRUE
				);

			legend.grob.height <- convertHeight(
				grobHeight(legend.grob.final),
				unitTo = 'lines',
				valueOnly = TRUE
				);

			# Create new grid grob to hold centered legend with border
			border.layout <- grid.layout(
				nrow = 1,
				ncol = 1,
				widths = unit(legend.grob.width + border.padding, 'lines'),
				heights = unit(legend.grob.height + border.padding, 'lines')
				);

			border.grob <- frameGrob(layout = border.layout);

			# Place legend grob
			border.grob <- placeGrob(
				frame = border.grob,
				grob = legend.grob.final,
				row = 1,
				col = 1
				);

			# Add border
			border.grob <- placeGrob(
				frame = border.grob,
				grob = rectGrob(
					gp = do.call(gpar, border)
					),
				row = NULL,
				col = NULL
				);

			# Update return object
			legend.grob.final <- border.grob;
			}

		# Close the device if it was created by draw.key
		if (!devices.open) {
			dev.off();

			# Remove the empty Rplots.pdf file if one was created (non-interactive)
			if (file.exists('Rplots.pdf')) {
				unlink('Rplots.pdf');
				}
			}
		}

	return(legend.grob.final);
	}
