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
create.multiplot <- function(plot.objects, filename = NULL, panel.heights = c(1,1), panel.widths = 1, main = NULL, main.cex = 2, main.key.padding = 1, ylab.padding = 5, xlab.padding = 5, xlab.to.xaxis.padding = 2, right.padding = 1, left.padding = 1, top.padding = 0.5, bottom.padding = 0.5, xlab.label = NULL, ylab.label = NULL, xlab.cex = 1.5, ylab.cex = 1.5, xaxis.cex = 2, yaxis.cex = 2, xaxis.labels = TRUE, yaxis.labels = TRUE, xaxis.alternating = 1, yaxis.alternating = 1, xat = TRUE, yat = TRUE, xlimits = NULL, ylimits = NULL, xaxis.rot = 0, xaxis.fontface = 'bold', yaxis.fontface = 'bold', xspacing = 1, yspacing = 1, x.relation = 'same', y.relation = 'same', xaxis.tck = c(0.75,0.75), yaxis.tck = c(0.75,0.75), axes.lwd = 1.5, key.right.padding = 1, key.left.padding = 1, key.bottom.padding = 1, height = 6, width = 6, size.units = 'in', resolution = 1000, enable.warnings = FALSE, key = list(text = list(lab = c(''))), legend =  NULL, print.new.legend = FALSE, merge.legends = FALSE, plot.layout = c(1,length(plot.objects)), layout.skip=rep(FALSE,length(plot.objects)), description = NULL, retrieve.plot.labels = FALSE) {

	# check that plots are trellis objects
	for (i in 1:length(plot.objects)) {
		if (class(plot.objects[[i]]) != 'trellis') {
			stop('Please only use trellis objects for this function');
			}
		}

	# combine plot objects together and set layout
	combined.plot.objects <- c(plot.objects[[1]], layout = plot.layout);
        if(length(plot.objects) > 1) {
		for (i in 2:length(plot.objects)) {
			combined.plot.objects <- c(combined.plot.objects, plot.objects[[i]], layout = plot.layout, merge.legends = merge.legends);
			}
		}

	# specify tck marks for different alternating settings
	if (0 == xaxis.alternating) { xaxis.tck <- c(0,0); }
	else if (1 == xaxis.alternating) { xaxis.tck[2] <- 0; }
	else if (2 == xaxis.alternating) { xaxis.tck[1] <- 0; }

	x.scale <- list(
		alternating = xaxis.alternating,
		tck = xaxis.tck,
		labels = xaxis.labels,
		cex = xaxis.cex,
		at = xat,
		rot = xaxis.rot,
		limits = xlimits,
		fontface = xaxis.fontface
		);

	# specify tck marks for different alternating settings
	if (0 == yaxis.alternating) { yaxis.tck <- c(0,0); }
	else if (1 == yaxis.alternating) { yaxis.tck[2] <- 0; }
	else if (2 == yaxis.alternating) { yaxis.tck[1] <- 0; }

	y.scale <- list(
		alternating = yaxis.alternating,
		tck = yaxis.tck,
		labels = yaxis.labels,
		cex = yaxis.cex,
		at = yat,
		rot = 0,
		limits = ylimits,
		fontface = yaxis.fontface
		);

	# if user asked to retrieve previous plot labels
	if (retrieve.plot.labels == TRUE) {
		y.relation <- 'free'
		x.relation <- 'free'
		y.scale$relation <- y.relation;
		x.scale$relation <- x.relation;
		}

	y.same <- FALSE;

	if ((typeof(yaxis.labels) != 'list') && (typeof(yat) != 'list') && (length(ylab.label) < 2)) {

		if (0 == yaxis.alternating) { yaxis.tck <- c(0,0); }
		else if (1 == yaxis.alternating) { yaxis.tck[2] <- 0; }
		else if (2 == yaxis.alternating) { yaxis.tck[1] <- 0; }

		y.scale <- list(
			alternating = yaxis.alternating,
			relation = y.relation,	
			tck = yaxis.tck,
			rot = 0,
			labels = yaxis.labels,
			cex = yaxis.cex,
			at = yat,
			limits = ylimits,
			fontface = yaxis.fontface
			);

		y.same <- TRUE;
		}

	if (y.same && (typeof(xaxis.labels) != 'list') && (typeof(xat) != 'list') && (length(xlab.label) < 2)) {

		x.scale <- list(
			alternating = xaxis.alternating,
			relation = x.relation,
			tck = xaxis.tck,
			labels = xaxis.labels,
			cex = xaxis.cex,
			at = xat,
			limits = xlimits,
			rot = xaxis.rot,
			fontface = xaxis.fontface
			);
		}

	trellis.object <- update(
		combined.plot.objects,
		relation = 'free',
		skip = layout.skip,
		between = list(y = yspacing, x = xspacing),
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
		main = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = main,
				fontface = 'bold',
				cex = main.cex
				)
			),
		xlab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = xlab.label,
				fontface = 'bold',
				cex = xlab.cex
				)
			),
		ylab = BoutrosLab.plotting.general::get.defaults(
			property = 'fontfamily', 
			add.to.list = list(
				label = rev(ylab.label),
				fontface = 'bold',
				cex = ylab.cex
				)
			),
		par.settings = list(
			axis.line = list(
				lwd = axes.lwd
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
        
        if (retrieve.plot.labels) {
			xaxis.labels = list();
			yaxis.labels = list();
			xat = list();
			yat = list();
			xlimits = list();
			ylimits = list();
			
			for (p in c(1:length(plot.objects))) {
				xlimits[[p]] = plot.objects[[p]]$x.limits;

   				if (length(plot.objects[[p]]$x.scales$labels) > 0) {
   					xaxis.labels[[p]] = plot.objects[[p]]$x.scales$labels;
   					}
				else { xaxis.labels[[p]] = TRUE; }
				
				xat[[p]] = plot.objects[[p]]$x.scales$at;
				ylimits[[p]] = plot.objects[[p]]$y.limits;
   				
   				if (length(plot.objects[[p]]$y.scales$labels) > 0) {
   					yaxis.labels[[p]] = plot.objects[[p]]$y.scales$labels;
   					}
				else { yaxis.labels[[p]] = TRUE; }
				yat[[p]] = plot.objects[[p]]$y.scales$at;
				}

			trellis.object$x.limits = xlimits;
			trellis.object$y.limits = ylimits;
			trellis.object$x.scales$at = xat;
			trellis.object$y.scales$at = yat;
			trellis.object$x.scales$labels = xaxis.labels;
			trellis.object$y.scales$labels = yaxis.labels;
			}

    # There is a glitch in update.trellis that prevents us from declaring multiple 'inside' legends
    # To get around this, we'll add in a special case to just set the 'legend' manually
    if (sum(names(legend) == "inside", na.rm = TRUE) > 1) {
        trellis.object$legend = legend;
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
