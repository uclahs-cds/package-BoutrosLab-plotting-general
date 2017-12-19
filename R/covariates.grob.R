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

### FUNCTION TO CREATE A COVARIATE BAR GROB ########################################################
covariates.grob <- function(
	covariates, ord, side = 'right', size = 1, grid.row = NULL, grid.col = NULL, grid.border = NULL,
	row.lines = NULL, col.lines = NULL, reorder.grid.index = FALSE, x = 0.5, y =0.5
	) {

	# This function creates a grid graphical object representing a covariate bar
	# It is adapted from the function latticeExtra:::dendrogramGrob

	# Remove padding so the covariate bars line up properly with the axes
	lattice.old.factor <- lattice.getOption('axis.padding')$factor;
	lattice.options('axis.padding' = list(factor = 0.5));

	# Calculate the scale for the covariate bars
	native.xscale <- c(1, length(ord)) + c(-1, 1) * lattice.getOption('axis.padding')$factor;
	native.unit <- 1 / diff(native.xscale);

	ncovariates <- length(covariates);
	key.gf <- NULL;

	# Create grob for either the right or top side of the image
	switch(
		side,
		right = {
			# Create the layout for the grob at the right of the image
			key.layout <- grid.layout(
				nrow = 1,
				ncol = ncovariates,
				heights = unit(1, 'null'),
				widths = unit(
					x = c(rep(size, ncovariates)),
					units = c(rep('lines', ncovariates))
					),
				respect = FALSE
				);

			# Create a frame using this layout
			key.gf <- frameGrob(layout = key.layout);

			# Place each of the covariate bars
			for (i in seq_len(ncovariates)) {
				covariatesi <- covariates[[i]];
				typei <- names(covariates)[i];
				switch(
					typei,
					rect = {
						key.gf <- placeGrob(
							frame = key.gf,
							grob = rectGrob(
								y = (order(ord) - native.xscale[1]) * native.unit,
								height = native.unit,
								gp = do.call(gpar, covariatesi)
								),
							row = 1,
							col = i
							);
						}
					);
				}

			# Draw row grid lines
			if (!is.null(grid.row)) {
				if (!is.list(grid.row)) {
					stop('Argument grid.row of covariates.grob must be a list of arguments to gpar.');
					}

				# By default, draw all row lines
				if (is.null(row.lines)) {
					row.lines <- 0:length(ord);
					}

				# Make sure user-specified lines are in bounds
				else if (any(row.lines < 0) || any(row.lines > length(ord))) {
					stop('Argument row.lines of covariates.grob out of bounds.');
					}

				for (i in row.lines) {
					index <- if (reorder.grid.index) { order(ord)[i] } else { i };
					key.gf <- placeGrob(
						frame = key.gf,
						grob = linesGrob(
							x = c(0, 1),
							y = if (i == 0) { c(0, 0) } else { rep(index * native.unit, 2) },
							gp = do.call(gpar, grid.row)
							),
						row = 1,
						col = NULL
						);
					}
				}

			# Draw column grid lines
			if (!is.null(grid.col)) {
				if (!is.list(grid.col)) {
					stop('Argument grid.col of covariates.grob must be a list of arguments to gpar.');
					}

				# By default, draw all column lines
				if (is.null(col.lines)) {
					col.lines <- 0:ncovariates;
					}

				# Make sure user-specified lines are in bounds
				else if (any(col.lines < 0) || any(col.lines > ncovariates)) {
					stop('Argument col.lines of covariates.grob out of bounds.');
					}

				for (i in col.lines) {
					key.gf <- placeGrob(
						frame = key.gf,
						grob = linesGrob(
							x = if (i == 0) { c(0, 0) } else { c(1, 1) },
							y = c(0, 1),
							gp = do.call(gpar, grid.col)
							),
						row = 1,
						col = if (i == 0) { 1 } else { i }
						);
					}
				}

			# Draw border
			if (!is.null(grid.border)) {
				if (!is.list(grid.border)) {
					stop('Argument grid.border of covariates.grob must be a list.');
					}

				grid.border[['fill']] <- 'transparent';

				key.gf <- placeGrob(
					frame = key.gf,
					grob = rectGrob(
						gp = do.call(gpar, grid.border)
						),
					row = NULL,
					col = NULL
					);
				}
			},
		top = {
			# Create the layout for the grob at the top of the image
			key.layout <- grid.layout(
				nrow = ncovariates,
				ncol = 1,
				widths = unit(1, 'null'),
				heights = unit(
					x = c(rep(size, ncovariates)),
					units = c(rep('lines', ncovariates))
					),
				respect = FALSE
				);

			# Create a frame using this layout
			key.gf <- frameGrob(layout = key.layout);

			# Place each of the covariate bars
			for (i in seq_len(ncovariates)) {
				covariatesi <- covariates[[i]];
				typei <- names(covariates)[i];
				switch(
					typei,
					rect = {
						key.gf <- placeGrob(
							frame = key.gf,
							grob = rectGrob(
								x = (order(ord) - native.xscale[1]) * native.unit,
								width = native.unit,
								gp = do.call(gpar, covariatesi)
								),
							row = i,
							col = 1
							);
						}
					);
				}

			# Draw column grid lines
			if (!is.null(grid.col)) {
				if (!is.list(grid.col)) {
					stop('Argument grid.col of covariates.grob must be a list.');
					}

				# By default, draw all column lines
				if (is.null(col.lines)) {
					col.lines <- 0:length(ord);
					}

				# Make sure user-specified lines are in bounds
				else if (any(col.lines < 0) || any(col.lines > length(ord))) {
					stop('Argument col.lines of covariates.grob out of bounds.');
					}

				for (i in col.lines) {
					index <- if (reorder.grid.index) { order(ord)[i] } else { i };
					key.gf <- placeGrob(
						frame = key.gf,
						grob = linesGrob(
							x = if (i == 0) { c(0, 0) } else { rep(index * native.unit, 2) },
							y = c(0, 1),
							gp = do.call(gpar, grid.col)
							),
						row = NULL,
						col = 1
						);
					}
				}

			# Draw row grid lines
			if (!is.null(grid.row)) {
				if (!is.list(grid.row)) {
					stop('Argument grid.row of covariates.grob must be a list.');
					}

				# By default, draw all row lines
				if (is.null(row.lines)) {
					row.lines <- 0:ncovariates;
					}

				# Make sure user-specified lines are in bounds
				else if (any(row.lines < 0) || any(row.lines > ncovariates)) {
					stop('Argument row.lines of covariates.grob out of bounds.');
					}

				for (i in row.lines) {
					key.gf <- placeGrob(
						frame = key.gf,
						grob = linesGrob(
							x = c(0, 1),
							y = if (i == 0) { c(0, 0) } else { c(1, 1) },
							gp = do.call(gpar, grid.row)
							),
						row = if (i == 0) { 1 } else { i },
						col = 1
						);
					}
				}

			# Draw border
			if (!is.null(grid.border)) {
				if (!is.list(grid.border)) {
					stop('Argument grid.border of covariates.grob must be a list.');
					}

				grid.border[['fill']] <- 'transparent';

				key.gf <- placeGrob(
					frame = key.gf,
					grob = rectGrob(
						gp = do.call(gpar, grid.border)
						),
					row = NULL,
					col = NULL
					);
				}
			}
		);

	# Restore original lattice setting
	lattice.options('axis.padding' = list(factor = lattice.old.factor));
	# set x and y coordinates
	key.gf$framevp$y <- unit(y, 'npc');
	key.gf$framevp$x <- unit(x, 'npc');
	# Return the grob
	return(key.gf);
	}
