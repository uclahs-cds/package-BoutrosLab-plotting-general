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

### FUNCTION TO CREATE MULTIPLOT ###################################################################
create.multiplot <- function(plot.objects, filename = NULL, panel.heights = c(1,1), panel.widths = 1, main = NULL, main.just = 'center', main.x = 0.5, main.y = 0.5, main.cex = 3, main.key.padding = 1, ylab.padding = 5, xlab.padding = 5, xlab.to.xaxis.padding = 2, right.padding = 1, left.padding = 1, top.padding = 0.5, bottom.padding = 0.5, xlab.label = NULL, ylab.label = NULL, xlab.cex = 2, ylab.cex = 2,xlab.top.label = NULL,xlab.top.cex = 2, xlab.top.col = 'black', xlab.top.just = "center",xlab.top.x = 0.5, xlab.top.y = 0, xaxis.cex = 1.5, yaxis.cex = 1.5, xaxis.labels = TRUE, yaxis.labels = TRUE, xaxis.alternating = 1, yaxis.alternating = 1, xat = TRUE, yat = TRUE, xlimits = NULL, ylimits = NULL, xaxis.rot = 0, xaxis.fontface = 'bold', yaxis.fontface = 'bold', x.spacing = 1, y.spacing = 1, x.relation = 'same', y.relation = 'same', xaxis.tck = c(0.75,0.75), yaxis.tck = c(0.75,0.75), axes.lwd = 1.5, key.right.padding = 1, key.left.padding = 1, key.bottom.padding = 1, height = 6, width = 6, size.units = 'in', resolution = 1600, enable.warnings = FALSE, key = list(text = list(lab = c(''))), legend =  NULL, print.new.legend = FALSE, merge.legends = FALSE, plot.layout = c(1,length(plot.objects)), layout.skip=rep(FALSE,length(plot.objects)), description = 'Created with BoutrosLab.plotting.general', retrieve.plot.labels = FALSE, plot.labels.to.retrieve = NULL, style = 'BoutrosLab', remove.all.border.lines = FALSE, preload.default = 'custom', plot.for.carry.over.when.same = 1) {

	if(preload.default == 'paper'){

		}
	else if(preload.default == 'web'){

		}
	# check that plots are trellis objects
	for (i in 1:length(plot.objects)) {
		if (class(plot.objects[[i]]) != 'trellis') {
			stop('Please only use trellis objects for this function');
			}
		}

	# Collect the axis limites, labels and at from all the plots
	xaxis.labels.plots <- list()[1:length(plot.objects)];
	yaxis.labels.plots <- list()[1:length(plot.objects)];
	xat.plots <- list()[1:length(plot.objects)];
	yat.plots <- list()[1:length(plot.objects)];
	xlimits.plots <- list()[1:length(plot.objects)];
	ylimits.plots <- list()[1:length(plot.objects)];

	# combine plot objects together and set layout
	combined.plot.objects <- c(plot.objects[[1]], layout = plot.layout);
	if(length(plot.objects) > 1) {
		for (i in 1:length(plot.objects)) {
			if(i > 1) {
				combined.plot.objects <- c(combined.plot.objects, plot.objects[[i]], layout = plot.layout, merge.legends = merge.legends);
				}
			# record each plot's axis values
			if(length(plot.objects[[i]]$x.scales$labels) > 0) {
				xaxis.labels.plots[[i]] <- plot.objects[[i]]$x.scales$labels;
				}
			if(length(plot.objects[[i]]$y.scales$labels) > 0) {
				yaxis.labels.plots[[i]] <- plot.objects[[i]]$y.scales$labels;
				}
		       	xat.plots[[i]] <- plot.objects[[i]]$x.scales$at;
		       	yat.plots[[i]] <- plot.objects[[i]]$y.scales$at;
			xlimits.plots[[i]] <- plot.objects[[i]]$x.limits;
		       	ylimits.plots[[i]] <- plot.objects[[i]]$y.limits;
			}
		}

	# specify tck marks for different alternating settings
	if (0 == xaxis.alternating) { xaxis.tck <- c(0,0); }
	else if (1 == xaxis.alternating) { xaxis.tck[2] <- 0; }
	else if (2 == xaxis.alternating) { xaxis.tck[1] <- 0; }

	# specify tck marks for different alternating settings
	if (0 == yaxis.alternating) { yaxis.tck <- c(0,0); }
	else if (1 == yaxis.alternating) { yaxis.tck[2] <- 0; }
	else if (2 == yaxis.alternating) { yaxis.tck[1] <- 0; }

	# if there are axis labels or tck locations for each plot, then the relations need to be free
	if ((typeof(yaxis.labels) == 'list') && (typeof(yat) == 'list')) {
		y.relation <- 'free';
		}
	if ((typeof(xaxis.labels) == 'list') && (typeof(xat) == 'list')) {
		x.relation <- 'free';
		}

	# if user asked to retrieve previous plot labels, then the relations need to be free
	if (is.logical(retrieve.plot.labels) && retrieve.plot.labels == TRUE) {
		y.relation <- 'free';
		x.relation <- 'free';
		}

	# if the limits, at and axis labels are not specified then carry them forward from the plots
	if(is.null(xlimits)) {
		xlimits <- if('same' == x.relation){ xlimits.plots[[plot.for.carry.over.when.same]]} else {xlimits.plots};
		}
	if(!is.null(xat) && !is.na(xat) && 1 == length(xat) && xat == TRUE) {
		xat <- if('same' == x.relation){xat.plots[[plot.for.carry.over.when.same]]} else {xat.plots};
		}
	if(!is.null(xaxis.labels) && !is.na(xaxis.labels) && 1 == length(xaxis.labels) && xaxis.labels == TRUE) {
		xaxis.labels <- if('same' == x.relation){xaxis.labels.plots[[plot.for.carry.over.when.same]]} else {xaxis.labels.plots};
		}
	if(is.null(ylimits)) {
		ylimits <- if('same' == y.relation){ ylimits.plots[[plot.for.carry.over.when.same]]} else {ylimits.plots};
		}
	if(!is.null(yat) && !is.na(yat) && 1 == length(yat) && yat == TRUE) {
		yat <- if('same' == y.relation){yat.plots[[plot.for.carry.over.when.same]]} else {yat.plots};
		}
	if(!is.null(yaxis.labels) && !is.na(yaxis.labels) && 1 == length(yaxis.labels) && yaxis.labels == TRUE) {
		yaxis.labels <- if('same' == y.relation){yaxis.labels.plots[[plot.for.carry.over.when.same]]} else {yaxis.labels.plots};
		}

	# consolidate all the parameters together for updating the lattice 
	x.scale <- list(
		alternating = xaxis.alternating,
		tck = xaxis.tck,
		labels = xaxis.labels,
		cex = xaxis.cex,
		at = xat,
		rot = xaxis.rot,
		limits = xlimits,
		fontface = if ('Nature' == style){'plain'} else(xaxis.fontface),
		relation = x.relation
		);
	y.scale <- list(
		alternating = yaxis.alternating,
		tck = yaxis.tck,
		labels = yaxis.labels,
		cex = yaxis.cex,
		at = yat,
		rot = 0,
		limits = ylimits,
		fontface = if ('Nature' == style){'plain'} else(yaxis.fontface),
		relation = y.relation
		);

	trellis.object <- update(
		combined.plot.objects,
		relation = 'free',
		skip = layout.skip,
		between = list(y = y.spacing, x = x.spacing),
		scales = list(
			x = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				add.to.list = x.scale
				),
			y = BoutrosLab.plotting.general::get.defaults(
				property = 'fontfamily',
				add.to.list = y.scale
				)
			),
		main =  BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = main,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = main.cex,
				just = main.just,
				x = main.x,
				y = main.y
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = xlab.label,
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = xlab.cex
				)
			),
		xlab.top = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily',
			add.to.list = list(
				label = xlab.top.label,
				cex = xlab.top.cex,
				col = xlab.top.col,
				fontface = if('Nature' == style){'plain'}else{'bold'},
				just = xlab.top.just,
				x = xlab.top.x,
				y = xlab.top.y
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = rev(ylab.label),
				fontface = if ('Nature' == style){'plain'} else('bold'),
				cex = ylab.cex
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd,
				col = if ('Nature' == style){'transparent'} else('black')
				),
			layout.heights = list(
				panel = rev(panel.heights),
				top.padding = top.padding,
				main = if (is.null(main)) { 0.3 } else { 1 },
				main.key.padding =  main.key.padding,
				key.top = 0.1,
				key.axis.padding = 0.1,
				axis.top = 0.7,
				axis.bottom = 1.0,
				axis.xlab.padding = xlab.to.xaxis.padding,
				xlab = 1,
				xlab.key.padding = 0.5,
				key.bottom = key.bottom.padding,
				key.sub.padding = 0.1,
				sub = 0.1,
				bottom.padding = bottom.padding
				),
			layout.widths = list(
				left.padding = left.padding,
				key.left = key.left.padding,
				key.ylab.padding = 0.5,
				ylab = 1,
				ylab.axis.padding = ylab.padding,
				xlab.axis.padding = xlab.padding,
				axis.left = 1,
				axis.panel = 0.3,
				strip.left = 0.3,
				panel = panel.widths,
				between = 1,
				axis.right = 1,
				axis.key.padding = 1,
				key.right = key.right.padding,
				right.padding = right.padding
				)
			),
		key = key,
		legend = if (print.new.legend) {legend} else {combined.plot.objects$legend}
		);

	# update above doesn't seem to go through so force it here
	trellis.object$x.limits = xlimits;
	trellis.object$y.limits = ylimits;

	# pulling forward a combination of axis limits, at and labels from the individual plots and the values passed to multiplot as a argument to the created mutliplot
	if (is.logical(retrieve.plot.labels) && retrieve.plot.labels == TRUE) {
		Nxaxis.labels <- list();
		Nyaxis.labels <- list();
		Nxat <- list();
		Nyat <- list();
		Nxlimits <- list();
		Nylimits <- list();

		for (p in c(1:length(plot.objects))) {
			# if the plot is listed in plot.labels.to.retrieve, use the values from the individual plots
			if(is.null(plot.labels.to.retrieve) || p %in% plot.labels.to.retrieve){
				Nxlimits[[p]] <- plot.objects[[p]]$x.limits;
				Nxat[[p]] <- plot.objects[[p]]$x.scales$at;
				Nylimits[[p]] <- plot.objects[[p]]$y.limits;
				Nyat[[p]] <- plot.objects[[p]]$y.scales$at;
				if (length(plot.objects[[p]]$x.scales$labels) > 0) {
					Nxaxis.labels[[p]] <- plot.objects[[p]]$x.scales$labels;
					}
				else { 
					Nxaxis.labels[[p]] <- TRUE; 
					}
				if (length(plot.objects[[p]]$y.scales$labels) > 0) {
					Nyaxis.labels[[p]] <- plot.objects[[p]]$y.scales$labels;
					}
				else { 
					Nyaxis.labels[[p]] <- TRUE; 
					}
				}
			# if the plot is not listed in plot.labels.to.retrieve, use the values pass to multiplot as arguments
			else if(!is.null(plot.labels.to.retrieve)){
				Nxlimits[[p]] <- xlimits[[p]];
				Nxat[[p]] <- xat[[p]];
				Nylimits[[p]] <- ylimits[[p]];
				Nyat[[p]] <- yat[[p]];
				if (length(plot.objects[[p]]$x.scales$labels) > 0) {
					Nxaxis.labels[[p]] <- plot.objects[[p]]$x.scales$labels;
					}
				else {
					Nxaxis.labels[[p]] <- TRUE;
					}
				if (length(plot.objects[[p]]$y.scales$labels) > 0) {
					Nyaxis.labels[[p]] <- plot.objects[[p]]$y.scales$labels;
					}
				else {
					Nyaxis.labels[[p]] = TRUE;
					}
				}

			}
		trellis.object$x.limits = Nxlimits;
		trellis.object$y.limits = Nylimits;
		trellis.object$x.scales$at = Nxat;
		trellis.object$y.scales$at = Nyat;
		trellis.object$x.scales$labels = Nxaxis.labels;
		trellis.object$y.scales$labels = Nyaxis.labels;
		}

	# There is a glitch in update.trellis that prevents us from declaring multiple 'inside' legends
	# To get around this, we'll add in a special case to just set the 'legend' manually
	if (sum(names(legend) == "inside", na.rm = TRUE) > 1) {
		trellis.object$legend <- legend;
		} 
    
	# If flag set to TRUE for removing all border lines, reset panel border lines
	# TODO: RSUN, allow custom setting to redraw certain border lines
	if (remove.all.border.lines) {
       		trellis.object <- update(
       			trellis.object,
       			reference = FALSE,
       			par.settings = list(
				axis.line = list(
					col = 0,
					scales=list(col=0,tck=c(0,0)),
 					panel=function(...){
 						lims <- current.panel.limits();
						panel.abline(h=lims$ylim[1],v=lims$xlim[1], col=0);
						}
					)
				)
			)
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
