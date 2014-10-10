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
show.available.palettes <- function(type = 'general', filename = NULL, height = 8, width = 12, resolution = 300){

	type <- tolower(type);

	### DISPLAY GENERAL SCHEMES ###################################################################
	# The general schemes are those found in the default.colours function
	if ('general' == type) {

		# Get the default colours
		general <- default.colours(palette.type = 'all');
		all.colours <- stack(general)$values;

		# Encoding the colours as numbers
		max.length <- 0;

		for ( i in 1:length(general)) {
			if(length(general[[i]]) > max.length) {
				max.length <- length(general[[i]])
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

		for (i in 1:length(general)){

			if (length(general[[i]]) < max.length){
				general[[i]] <- c(general[[i]], rep("NA", max.length - length(general[[i]])));
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
			yaxis.cex = 2,
			yaxis.fontface = 1,
			height = height,
			width = width,
			resolution = resolution	
			);
		}

	### DISPLAY SPECIFIC SCHEMES ##################################################################

	else if ('specific' == type) {

		specific <- force.colour.scheme(x = '', scheme = 'all', return.scheme = TRUE); 

		# sort specific by length
		swapped = TRUE;
		while (TRUE == swapped) {
			swapped = FALSE;
			for (i in 2:length(specific)) {
				if (length(specific[[i-1]]$colours) > length(specific[[i]]$colours)) {
					temp <- specific[i];
					temp.name <- names(specific)[i];
					specific[i] <- specific[i - 1];
					names(specific)[i] <- names(specific)[i - 1];
					specific[i - 1] <- temp;
					names(specific)[i - 1] <- temp.name;
					swapped = TRUE;
					}
				}
			}

		# this layout makes certain assumptions:
			# 1. one scheme is significantly longer than the others (ex. tissue) and should be on its own
			# 2. other schemes lain out in order will approximately add to equal the length of the longest scheme -- this determines the number of columns
		# Therefore, with the addition of more schemes, this layout may not look very good, and may need tweaking.

		# separate the longest legend to be on its own
		longest.legend <- list(
			legend = list(
				colours = specific[[length(specific)]]$colours,
				labels = specific[[length(specific)]]$levels,
				border = "white",
				title = names(specific)[length(specific)]
				)
			);

		intermediate <- list();

		for (i in 1:(length(specific) - 1)) {
			intermediate <- append(intermediate, list(
				legend = list(
					colours = specific[[i]]$colours,
					labels = specific[[i]]$levels,
					border = "white",
					title = names(specific)[i]
					)
				)
				)
			}
		
		# The number of rows in the final layout
		layout.multiple <- 3;
	
		legend.half <- legend.grob(
			legends = intermediate,
			title.just = "left",
			layout = c(ceiling(length(intermediate)/layout.multiple),layout.multiple)
			);

		long.legend.grob <- legend.grob(
			legends = longest.legend,
			title.just = "left"
			);

		legend.layout <- grid.layout(
			nrow = 1,
			ncol = 3,
			widths = unit(
				x = c(0, 0.5, 0), 
				units = "lines"),
			heights = unit(
				x = 1,
				units = "npc"
				),
			just = "top"
			);

		final.grob <- frameGrob(layout = legend.layout);

		final.grob <- packGrob(
			frame = final.grob,
			grob = long.legend.grob,
			row = 1,
			col = 1
			);

		final.grob <- packGrob(
			frame = final.grob,
			grob = legend.half,
			row = 1,
			col = 3
			);
	
		# create empty plotting space
		# blank plot
		blank.data <- data.frame(
			a = c(0,0),
			b = c(0,0))
		filler <- BoutrosLab.plotting.general::create.barplot(
			formula = a ~ b,
			data = blank.data,
			col = "white",
			lwd = 0,
			xaxis.lab = "",
			yaxis.lab = ""
			)

		BoutrosLab.plotting.general::create.multiplot(
			plot.objects = list(filler, filler),
			axes.lwd = 0,
			xaxis.lab = NULL,
			yaxis.lab = NULL,
			filename = filename,
			legend = list(
				inside = list(
					fun = final.grob
					)
				),
			print.new.legend = TRUE,
			resolution = resolution,
			height = 8,
			width = 12
			)
		}

	else (
		stop("Invalid value supplied to type parameter")
		)

	}
