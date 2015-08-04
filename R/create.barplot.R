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

### FUNCTION TO CREATE BARPLOTS ###################################################################
create.barplot <- function(formula, data, groups = NULL, stack = FALSE, filename = NULL, main = NULL, abline.h = NULL, abline.v = NULL, abline.type = NULL, abline.lwd = NULL, abline.col = 'black', add.grid = FALSE, xgrid.at = xat, ygrid.at = yat, grid.lwd = 5, xlab.label = tail(sub('~','',formula[-2]),1), ylab.label = tail(sub('~','',formula[-3]),1), xlab.col = 'black', ylab.col = 'black', xaxis.lab = TRUE, yaxis.lab = TRUE, xaxis.col = 'black', xaxis.fontface = 'bold', yaxis.fontface = 'bold', yaxis.col = 'black', xaxis.cex = 1.2, yaxis.cex = 1.5, xaxis.rot = 0, yaxis.rot = 0, xaxis.tck = 1, yaxis.tck = 1, main.cex = 2.5, xlab.cex = 2, ylab.cex = 2, ylimits = NULL, yat = TRUE, xlimits = NULL, xat = TRUE, x.spacing = 0, y.spacing = 0, top.padding = 0.5, bottom.padding = 1, right.padding = 1, left.padding = 1, key.bottom = 0.1, ylab.axis.padding = 0, axis.xlab.padding = 0.5, x.relation = 'same', y.relation = 'same', layout = NULL, as.table = FALSE, col = 'black', strip.col = 'white', strip.cex = 1, background.col = 'transparent', border.col = 'black', y.error.up = NULL, y.error.down = y.error.up, y.error.bar.col = 'black', error.whisker.width = width/(nrow(data)*4), error.bar.lwd = 1, error.whisker.angle = 90, add.background.shading = FALSE, background.shading.xpos = NULL, background.shading.ypos = NULL, background.shading.colour = 'grey85', raster = NULL, raster.vert = TRUE, raster.just = 'center', raster.width.dim = unit(2/37, 'npc'), key = list(text = list(lab = c(''))), legend = NULL, lwd = 1, plot.horizontal = FALSE, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, description = NULL,add.rectangle = FALSE, xleft.rectangle = NULL, ybottom.rectangle = NULL, xright.rectangle = NULL, ytop.rectangle = NULL, col.rectangle = 'transparent', alpha.rectangle = 1, origin = 0, reference = TRUE, sample.order = 'none', group.labels = FALSE, line.func = NULL, line.from = 0, line.to = 0, line.col = 'transparent', line.infront = TRUE, text.above.bars = list(labels = NULL, padding = NULL, bar.locations = NULL, rotation = 0), style = 'BoutrosLab') {

####### Error checking ########
   tryCatch({
        if (is.null(formula))
        {
            stop();
        }
        as.formula(formula);
    },error = function(message)
    {
        stop("Invalid formula.");  
    }
    );

    
    # Basic type check 
    

	# allow a gray spectrum if groups is specified and only a single colour is given
	groups.new <- eval(substitute(groups), data, parent.frame());
	if (!is.null(groups.new) && 1 == length(col) && col == 'grey') {
		col <- grey(1:nlevels(as.factor(groups.new)) / nlevels(as.factor(groups.new)));
		}

	trellis.object <- lattice::barchart(
		formula,
		data,
		panel = function(x, y, subscripts, groups = groups.new, ...) {
			if (!is.null(text.above.bars$labels)) {
				if (!is.null(groups.new)) {stop('text above bars does not work with grouped plots')}
				if (plot.horizontal) {
					panel.text(x[text.above.bars$bar.locations] + text.above.bars$padding,
						text.above.bars$bar.locations,
						text.above.bars$labels, 
						srt = text.above.bars$rotation)
					}
				else {
					panel.text(text.above.bars$bar.locations, 
						y[text.above.bars$bar.locations] + text.above.bars$padding,
						text.above.bars$labels, 
						srt = text.above.bars$rotation)
					}
				}

			# add rectangle	
			if (add.rectangle) {
				panel.rect(
					xleft = xleft.rectangle,
					ybottom = ybottom.rectangle,
					xright = xright.rectangle,
					ytop = ytop.rectangle,
					col = col.rectangle,
					alpha = alpha.rectangle,
					border = NA
					);
				}
			
			# add background shading
			if (add.background.shading) {
				if (!is.null(background.shading.xpos)) {
					if (length(background.shading.xpos)%%2 == 1) {
						xleft = background.shading.xpos[seq(1,length(background.shading.xpos)-1,2)];
						xright = background.shading.xpos[seq(2,length(background.shading.xpos)-1,2)];
						}
					else {
						xleft = background.shading.xpos[seq(1,length(background.shading.xpos),2)];
						xright = background.shading.xpos[seq(2,length(background.shading.xpos),2)];
						}
					}
				else {
					xleft = xlimits[1];
					xright = xlimits[2];
					}
				if (!is.null(background.shading.ypos)) {
					if (length(background.shading.ypos)%%2 == 1) {
						ybottom = background.shading.ypos[seq(1,length(background.shading.ypos)-1,2)];
						ytop = background.shading.ypos[seq(2,length(background.shading.ypos)-1,2)];
						}
					else {
						ybottom = background.shading.ypos[seq(1,length(background.shading.ypos),2)];
						ytop = background.shading.ypos[seq(2,length(background.shading.ypos),2)];
					}
				}
				else {
					ybottom = ylimits[1];
					ytop = ylimits[2];
					}

				panel.rect(
					xleft = xleft,
					ybottom = ybottom,
					xright = xright,
					ytop = ytop,
					col = background.shading.colour,
					border = 'transparent'
					);
				}

			# add grid-lines
			if (add.grid) {
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
					col = trellis.par.get('reference.line')$col,
					lwd = grid.lwd
					);
				}

			# add line
			if (length(line.func) > 0 && !line.infront) {
				panel.curve(expr = line.func, from = line.from, to = line.to,col = line.col);
				}

			panel.abline(h = abline.h, lty = abline.type, lwd = abline.lwd, col = abline.col);
			panel.abline(v = abline.v, lty = abline.type, lwd = abline.lwd, col = abline.col);

			panel.barchart(x, y, subscripts = subscripts, groups = groups.new, border = border.col, lwd = lwd, ..., origin = 0);

			if (length(line.func) > 0 && line.infront) {
				panel.curve(expr = line.func, from = line.from, to = line.to,col = line.col);
				}

			# add error bars
			if (!is.null(y.error.up)) {
				# handle x-position offset due to groups
				if (!is.null(groups)) {
					num.groups <- length(subscripts) / length(unique(groups));
					group.num  <- (subscripts - 1) %/% num.groups;
					xoffset <- error.whisker.width * (mean(groups) - 2 + group.num);
					}
				else {
					xoffset <- 0;
					}

				panel.arrows(
					x0 = x + xoffset,
					y0 = y + y.error.up,
					x1 = x + xoffset,
					y1 = y - y.error.down,
					length = error.whisker.width,
					angle = error.whisker.angle,
					ends = 'both', 
					col = y.error.bar.col,
					lwd = error.bar.lwd
					);
				}

			# add raster fill 
			if (!is.null(raster)) {
				if (raster.vert) {
					grid.raster(
						raster,
						x = x,
						y = 0,
						height = y,
						just = raster.just,
						default.units = 'native',
						width = raster.width.dim
						);
					}
				else {
					grid.raster(
						raster,
						x = 0,
						width = x,
						y = y,
						just = raster.just,
						default.units = 'native',
						height = raster.width.dim
						);
					}
				}
			},
		horizontal = plot.horizontal,
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = xlab.label,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = xlab.cex,
				col = xlab.col
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = ylab.label,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = ylab.cex,
				col = ylab.col
				)
			),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily', 
				add.to.list = list(
					labels = xaxis.lab,
					cex = xaxis.cex,
					rot = xaxis.rot,
					col = xaxis.col,
					limits = xlimits,
					at = xat,
					tck = xaxis.tck,
					relation = x.relation,
					fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
					alternating = FALSE
					)
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily', 
				add.to.list = list(
					labels = yaxis.lab,
					cex = yaxis.cex,
					rot = yaxis.rot,
					col = yaxis.col,
					limits = ylimits,
					at = yat,
					tck = yaxis.tck,
					relation = y.relation,
					fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
					alternating = FALSE
					)
				)
			),
		between = list(
			x = x.spacing,
			y = y.spacing
			),
		par.settings = list(
			axis.line = list(
				lwd = lwd,
				col = if ('Nature' == style){'transparent'} else('black')
				),
			layout.heights = list(
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3} else { 1 },
				main.key.padding = 0.1,
				key.top = 0.1,
				key.axis.padding = 0.1,
				axis.top = 0.7,
				axis.bottom = 1.0,
				axis.xlab.padding = axis.xlab.padding,
				xlab = 1,
				xlab.key.padding = 0.1,
				key.bottom = key.bottom,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = 0,
				key.ylab.padding = 0.5,
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
			strip.background = list(
				col = strip.col
				),
			panel.background = list(
				col = background.col
				)
			),
		par.strip.text = list(
			cex = strip.cex
			),
		col = col,
		layout = layout,
		as.table = as.table,	
		key = key,
		legend = legend,
		pretty = TRUE,
		stack = stack,
		reference = reference
		);

	# add grouped labels
	if (group.labels) {
		numGroups = length(trellis.object$panel.args[[1]]$x)/length(unique(trellis.object$panel.args[[1]]$x));
		intialaddition <- (1/3)/numGroups;
		additions <- intialaddition * 2;
		newxat <- NULL;
		
		if (is.logical(trellis.object$x.scales$at)) {
			trellis.object$x.scales$at = c(1:length(unique(trellis.object$panel.args[[1]]$x)));
			}
		
		for( i in trellis.object$x.scales$at) {
			for (j in c(1:numGroups)) {
				newxat <- c(newxat, i - 1/3 + intialaddition + additions*(j-1));
				}
			}
		
		trellis.object$x.scales$at = newxat;
		}
	
	# reorder the bars in decreasing or increasing order if specified
	if (is.null(sample.order) || is.na(sample.order)) { sample.order <- 'none'; }
	
	if (sample.order[1] != 'none') {
		for( i in 1:length(trellis.object$panel.args)) {
			# will need two separate ways for horizontal and non - horizontal
			if (!plot.horizontal) {
				num.bars <- length(unique(trellis.object$panel.args[[1]]$x));

				if (length(unique(sample.order)) == num.bars) {
					if (length(xaxis.lab) == 1 && xaxis.lab) {
						ordering <- rev(match(sample.order, trellis.object$panel.args[[i]]$x));
						}
					else { 
						ordering <- rev(match(sample.order, trellis.object$x.scales$labels));
						}
					}
				
				if (length(sample.order) == 1) {
					if(sample.order == 'decreasing') {
						# order is the new order the bars will appear in
						ordering <- order(trellis.object$panel.args[[1]]$y[c(1:num.bars)]);
						}
				
					# reverse order if increasing
					if (sample.order == 'increasing') { 
						ordering <- rev(order(trellis.object$panel.args[[1]]$y[c(1:num.bars)])) 
						}
					}
				
				# if label locations are specified, change them
				if (xat != TRUE) {
					newxat <- NULL
					for (j in rev(ordering)) {
						if (length(which(xat == j) > 0)) {
							newxat <- c(newxat,which(rev(ordering) == j));
							}
						else {newxat <- c(newxat,0);}
						}
					trellis.object$x.scales$at = newxat;
					}

				# if labels were not specified reorder the default ones
				if (length(xaxis.lab) == 1 && xaxis.lab) {
					trellis.object$x.scales$labels = rep(trellis.object$panel.args[[i]]$x[rev(ordering)],length(trellis.object$panel.args[[1]]$x)/num.bars)
					}
				# if labels are specified reorder the specified ones
				else {
					trellis.object$x.scales$labels = rev(trellis.object$x.scales$labels[rev(ordering)])
					warning("WARNING: the label order you specified has been reordered") 
					}

				for (j in 0:(length(trellis.object$panel.args[[1]]$x)/num.bars - 1)) {
					# reorder values of bars
					trellis.object$panel.args[[i]]$y[c((1+ j*num.bars):(num.bars*(j+1)))] = rev(trellis.object$panel.args[[i]]$y[ordering+num.bars*j])
					# reorder values of x to order in logical order
					trellis.object$panel.args[[i]]$x = rep(1:length(ordering),length(trellis.object$panel.args[[1]]$x)/num.bars)
					}
				
				}
			else {		

				num.bars <- length(unique(trellis.object$panel.args[[1]]$y));
				if (length(unique(sample.order)) == num.bars) {
					if (length(yaxis.lab) == 1 && yaxis.lab) {
						ordering <- match(sample.order, sort(sample.order)[trellis.object$panel.args[[i]]$y]);
						}
					else { 
						ordering <- match(sample.order, sort(sample.order)[trellis.object$y.scales$labels]);
						}
					}
				
				if (length(sample.order) == 1 && sample.order == 'decreasing') {
					ordering <- order(trellis.object$panel.args[[1]]$x[c(1:num.bars)]);
					}	
				
				if (length(sample.order) == 1 && sample.order == 'increasing') { 
					ordering <- rev(order(trellis.object$panel.args[[1]]$x[c(1:num.bars)]));
					}

				if (yat != TRUE) {
					newyat <- NULL;
					for (j in rev(ordering)) {
						if (length(which(yat == j) > 0)) {
							newyat <- c(newyat,which(rev(ordering) == j));
							}
						else {
							newyat <- c(newyat,0);
							}
						}
					trellis.object$y.scales$at = newyat;
					}

				if (length(yaxis.lab) == 1 && yaxis.lab) {
						trellis.object$y.scales$labels = rep(trellis.object$panel.args[[i]]$y[ordering],length(trellis.object$panel.args[[1]]$y)/num.bars)
					}
				else {
					trellis.object$y.scales$labels = rev(trellis.object$y.scales$labels[ordering])
					warning("WARNING: the label order you specified has been reordered") 
					}	
								
				for (j in 0:(length(trellis.object$panel.args[[1]]$y)/num.bars - 1)) {
					trellis.object$panel.args[[i]]$x[c((1+ j*num.bars):(num.bars*(j+1)))] = rev(trellis.object$panel.args[[i]]$x[ordering+num.bars*j])
					trellis.object$panel.args[[i]]$y = rep(1:length(ordering),length(trellis.object$panel.args[[1]]$y)/num.bars)
					}
				}
			}
			y.error.up <- y.error.up[rev(ordering)]
			y.error.down <- y.error.down[rev(ordering)]
		}


	# If Nature style requested, change figure accordingly
	if ('Nature' == style) {

		# Re-add bottom and left axes
		trellis.object$axis = function(side, line.col = "black", ...) {
			# Only draw axes on the left and bottom
			if(side %in% c("bottom","left")) {
				axis.default(side = side, line.col = "black", ...);
				lims <- current.panel.limits();
				panel.abline(h = lims$ylim[1], v = lims$xlim[1]);
				}
			}

		# Ensure sufficient resolution for graphs
		if (resolution < 1200) {
			resolution <- 1200;
			warning("Setting resolution to 1200 dpi.");
			}

		# Other required changes which are not accomplished here
		warning("Nature also requires italicized single-letter variables and en-dashes for ranges and negatives. See example in documentation for how to do this.");

		warning("Avoid red-green colour schemes, create TIFF files, do not outline the figure or legend")
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
