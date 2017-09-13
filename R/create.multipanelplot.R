create.multipanelplot<-function(plot.objects = NULL, filename = NULL,height = 10, width = 10, resolution = 1000, 
                            heights = c(rep(1,layout.height)),widths = c(rep(1,layout.width)), layout.width = 1, 
                            layout.height = length(plot.objects), main = '', main.padding = 2,main.x = 0.5, main.y = 0.5, x.spacing = 0, y.spacing= 0,
                            xlab.label = '', xlab.cex = 2, ylab.label = '', ylab.label.right = '', 
                            ylab.cex = 2, main.cex = 3, legend = NULL, left.padding = 3,  
                            ylab.axis.padding = 0,xlab.axis.padding = 0, bottom.padding = 0, top.padding = 0, right.padding = 0,
                            layout.skip = c(rep(FALSE, layout.width*layout.height)),
                            description = 'Created with BoutrosLab.plotting.general', size.units = 'in',enable.warnings = FALSE){
  
	### ERROR CHECKING ###
	if (length(heights) != layout.height) {
		stop("heights must have layout.height number of entries");
		}
	if (length(widths) != layout.width) {
		stop("widths must have layout.width number of entries");
		}
	if (length( layout.skip) != layout.width*layout.height) {
		stop("layout.skip must have same number of entries as layout.width * layout.height");
		}
		
	padding.text.to.padding.ratio <- 6; # this is used to align plots with diffrent label sizes
  	tick.to.padding.ratio <- 0.9484252; # this is used to evaluate length of ticks (is equivalent to 1mm)
	### HANDLE SKIP LAYOUT ###
	newplots <- list();
	plotnum <- 1;
	for (i in c(1:(layout.width*layout.height))) {
		if (plotnum <= length(plot.objects) && layout.skip[i] == FALSE) {
			newplots[i] <- plot.objects[plotnum];
			plotnum <- plotnum + 1;
			}
		else {
			newplots[[i]] <- rectGrob(gp=gpar(col="white", alpha = 0)); # make a white spacer grob
			}
  		}
  
	plot.objects <- newplots
  
	### PAR SETTINGS EQUIVALENT ###
	if (length(plot.objects) > 1) {
		for (i in c(1:length(plot.objects))) {
      			# make all paddings the same (in order to line them up)
			if (!is.null(plot.objects[[i]]$par.settings)) {


				plot.objects[[i]]$main$label <- ''
				plot.objects[[i]]$par.settings$layout.heights <- NULL;
				plot.objects[[i]]$par.settings$layout.widths <- NULL;
				plot.objects[[i]]$par.settings$layout.widths$left.padding <- 0;
				plot.objects[[i]]$par.settings$layout.widths$key.left <- 0;
				plot.objects[[i]]$par.settings$layout.widths$key.ylab.padding <- 0;
				plot.objects[[i]]$par.settings$layout.widths$ylab <- 0;
				plot.objects[[i]]$par.settings$layout.widths$ylab.axis.padding <-  ylab.axis.padding;
				plot.objects[[i]]$par.settings$layout.widths$axis.left <- 0;
				plot.objects[[i]]$par.settings$layout.widths$axis.right <- 0;
				plot.objects[[i]]$par.settings$layout.widths$axis.key.padding <- 0;
				plot.objects[[i]]$par.settings$layout.widths$key.right <- 0;
				plot.objects[[i]]$par.settings$layout.widths$right.padding <- 0.1;
				plot.objects[[i]]$par.settings$layout.heights$top.padding <- 0;
				plot.objects[[i]]$par.settings$layout.heights$main <- 0;
 				plot.objects[[i]]$par.settings$layout.heights$main.key.padding <- 0;
				plot.objects[[i]]$par.settings$layout.heights$key.top <- 0;
				plot.objects[[i]]$par.settings$layout.heights$key.axis.padding <- 0;
				plot.objects[[i]]$par.settings$layout.heights$axis.top <- 0;
				plot.objects[[i]]$par.settings$layout.heights$axis.bottom <- 0;
				plot.objects[[i]]$par.settings$layout.heights$axis.xlab.padding <- xlab.axis.padding;
				plot.objects[[i]]$par.settings$layout.heights$xlab <- 0;
				plot.objects[[i]]$par.settings$layout.heights$xlab.key.padding <- 0;
				plot.objects[[i]]$par.settings$layout.heights$key.bottom <- 0;
				plot.objects[[i]]$par.settings$layout.heights$key.sub.padding <- 0;
				plot.objects[[i]]$par.settings$layout.heights$sub <- 0;
				plot.objects[[i]]$par.settings$layout.heights$bottom.padding <- 0;
				}
      			}
		}
  
  
	# add appropriate paddings to specific plots
	for (i in c(1:length(plot.objects))) {
		if (!is.null(plot.objects[[i]]$par.settings)) {
	
      			if (i %in% seq(1,length(plot.objects),layout.width)) {
				plot.objects[[i]]$par.settings$layout.widths$left.padding <- left.padding;
				}
      
			if (i %in% seq(layout.width,length(plot.objects),layout.width)) {
				plot.objects[[i]]$par.settings$layout.widths$right.padding <- right.padding;
				}
      
			if (i %in% seq((layout.width*(layout.height) - (layout.width - 1)),length(plot.objects),1)) {
				plot.objects[[i]]$par.settings$layout.heights$bottom.padding <- bottom.padding;
				}
      
			plot.objects[[i]]$par.settings$layout.widths$ylab.axis.padding <- ylab.axis.padding;
			plot.objects[[i]]$par.settings$layout.heights$axis.xlab.padding <- xlab.axis.padding;
      
			if (i %in% seq(1,layout.width,1)) {
				plot.objects[[i]]$par.settings$layout.heights$top.padding <- top.padding;
				}
			}
		}

	### SPACING #####
	#make y spacing into an array of proper length
	if (length(y.spacing) == 1) {
		y.spacing <- rep(y.spacing,length(plot.objects));
		}
	else if (length(y.spacing) == layout.height) {
		y.spacing <- rep(y.spacing,layout.width, each = layout.width);
		}
	else {
		stop("y.spacing must be length 1, or same as layout.height");
		}
  
	#make x spacing into an array of proper length
	if (length(x.spacing) == 1) {
		x.spacing <- rep(x.spacing,length(plot.objects));
		}
	else if (length(x.spacing) == layout.width) {
		x.spacing <- rep(x.spacing,layout.height);
		}
	else {
		stop("x.spacing must be length 1, or same as layout.width");
		}
  
	# add spacing to par.settings
	for (i in c(1:length(plot.objects))) {	
		if (!is.null(plot.objects[[i]]$par.settings)) {
			plot.objects[[i]]$par.settings$layout.heights$top.padding <- plot.objects[[i]]$par.settings$layout.heights$top.padding + y.spacing[i];
			plot.objects[[i]]$par.settings$layout.widths$right.padding <- plot.objects[[i]]$par.settings$layout.widths$right.padding + x.spacing[i];
			}	
		}
  
	### PADDING ####
  
	#variables to represent amount of padding needed
	largest.y.axis <- list();
	largest.x.axis <- list();
	left.ticks <- list();
	bottom.ticks <- list();
	y.label.size <- list();
	x.label.size <- list();
	for (i in  c(1:length(plot.objects))) {
    
		largest.y.axis[i] <- 0;
		left.ticks[i] <- 0;
		largest.x.axis[i] <- 0;
		bottom.ticks[i] <- 0;
		x.label.size[i] <- 0;
		y.label.size[i] <- 0;
    		# set variables for each plot according to their respective plot values
		if (!is.null(plot.objects[[i]]$par.settings)) {
			y.label.size[i] <- get.text.grob.width(plot.objects[[i]]$ylab$label,plot.objects[[i]]$ylab$cex,90,filename, width,height,resolution);
			x.label.size[i] <- get.text.grob.height(plot.objects[[i]]$xlab$label,plot.objects[[i]]$xlab$cex,0,filename,width,height,resolution);
			
			if (plot.objects[[i]]$y.scales$labels == TRUE || is.null(plot.objects[[i]]$y.scales$labels)) {
				if (!is.null(plot.objects[[i]]$panel.args[[1]]$y)) {
					yvals = as.numeric(plot.objects[[i]]$panel.args[[1]]$y)
					y.axis.labs =  pretty(yvals[is.finite(yvals)])
					}
				else {
					y.axis.labs = ''
					}
				}
			else {
				y.axis.labs = plot.objects[[i]]$y.scales$labels
				}

			largest.y.axis[i] <- get.text.grob.width(y.axis.labs,
								plot.objects[[i]]$y.scales$cex, 
								plot.objects[[i]]$y.scales$rot[1], 
								filename, width,height,resolution);
			left.ticks[i] <- plot.objects[[i]]$y.scales$tck[1]*tick.to.padding.ratio;

			if (plot.objects[[i]]$x.scales$labels == TRUE || is.null(plot.objects[[i]]$x.scales$labels)) {
				if (!is.null(plot.objects[[i]]$panel.args[[1]]$x)) {
					xvals = as.numeric(plot.objects[[i]]$panel.args[[1]]$x)
					x.axis.labs =  pretty(xvals[is.finite(xvals)])
					}
				else{
					x.axis.labs = ''
					}
				}
			else {
				x.axis.labs = plot.objects[[i]]$x.scales$labels
				}

			largest.x.axis[i] <- get.text.grob.height(x.axis.labs,
								 plot.objects[[i]]$x.scales$cex, 
								 plot.objects[[i]]$x.scales$rot[1], 
								 filename, width,height,resolution);
      			bottom.ticks[i] <- plot.objects[[i]]$x.scales$tck[1]*tick.to.padding.ratio;
			}
		}
  
	# add the correct amount to each plot based on the column
	for ( i in c(1:layout.width)) {
		maxAxis <- max(unlist(largest.y.axis[seq(i,length(plot.objects),layout.width)]));
		maxTicks <- max(unlist(left.ticks[seq(i,length(plot.objects),layout.width)]));
		maxLabels <- max(unlist(y.label.size[seq(i,length(plot.objects),layout.width)]));
		for ( j in seq(i,length(plot.objects),layout.width)) {
			to.add <- (maxAxis + maxLabels) / padding.text.to.padding.ratio + maxTicks;
			if (!is.null(plot.objects[[j]]$par.settings)) {
				plot.objects[[j]]$par.settings$layout.widths$ylab.axis.padding <- plot.objects[[j]]$par.settings$layout.widths$ylab.axis.padding + to.add;
				}
			}
		}
  
  
	# add the correct amount to each plot based on the row
	for ( i in seq(1,length(plot.objects),layout.width)) {
		maxAxis <- max(unlist(largest.x.axis[seq(i,i + layout.width - 1,1)]));
		maxTicks <- max(unlist(bottom.ticks[seq(i,i + layout.width - 1,1)]));
		maxLabels <- max(unlist(x.label.size[seq(i,i + layout.width - 1,1)]));
		for ( j in c(i:(i + layout.width - 1))) {
			to.add <- (maxAxis + maxLabels) / padding.text.to.padding.ratio + maxTicks;
			if (j <= length(plot.objects) && !is.null(plot.objects[[j]]$par.settings)) {
				plot.objects[[j]]$par.settings$layout.heights$axis.xlab.padding <- plot.objects[[j]]$par.settings$layout.heights$axis.xlab.padding + to.add;
				}
			}
		}
  

	# adjust spacing and layout values for last row/column
	for (i in c(1:length(plot.objects))) {
		if (!is.null(plot.objects[[i]]$par.settings)) {
			# dont add spacing to last row
			if (i %in% seq(1,layout.width,1)) {
				plot.objects[[i]]$par.settings$layout.heights$top.padding <- top.padding;
				}
      
			# dont add spacing to last column
			if (i %% layout.width == 0) {
				plot.objects[[i]]$par.settings$layout.widths$right.padding <- right.padding;
				}
			}
		}

	#grobs representing main labels on each side
	main.label <- textGrob(main,gp=gpar(cex = main.cex, fontface = 'bold', fontfamily = 'Arial'), x = main.x, y = main.y);
	y.label <- textGrob(ylab.label,gp=gpar(cex = ylab.cex, fontface = 'bold', fontfamily = 'Arial'),rot = 90);
	y.label.right <- textGrob(ylab.label.right,gp=gpar(cex = ylab.cex, fontface = 'bold', fontfamily = 'Arial'), rot = -90);
	x.label <- textGrob(xlab.label,gp=gpar(cex = xlab.cex, fontface = 'bold', fontfamily = 'Arial'));
  
	### LEFT,RIGHT,TOP,BOTTOM GROBS ############
  
	### LEFT GROB
	if (!is.null(legend$left$fun)) { 
		width.legend <- convertUnit(
			grobWidth(legend$left$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		width.text <- convertUnit(
			grobWidth(y.label),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		left.layout.final <- grid.layout(
			nrow = 1,
			ncol = 2,
			widths = unit(
				x = c(width.legend, width.text),
				units = c('lines','lines')
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);
		left.grob <- frameGrob(layout = left.layout.final)
			left.grob <- placeGrob(
			frame = left.grob,
			grob = legend$left$fun,
			row = 1,
			col = 1
			);
		left.grob <- placeGrob(
			frame = left.grob,
			grob = y.label,
			row = 1,
			col = 2
			);
		}
	else {
		left.grob <- y.label;
		}
  
	### RIGHT GROB
	if (!is.null(legend$right$fun)) { 
		width.legend <- convertUnit(
			grobWidth(legend$right$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		width.text <- convertUnit(
			grobWidth(y.label.right),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		right.layout.final <- grid.layout(
			nrow = 1,
			ncol = 2,
			widths = unit(
				x = c(width.text, width.legend),
				units = c('lines','lines'),
				data = list(NULL, NULL)
				),
			heights = unit(1, 'null'),
			respect = FALSE
			);
    
		right.grob <- frameGrob(layout = right.layout.final)
		right.grob <- placeGrob(
			frame = right.grob,
			grob = y.label.right,
			row = 1,
			col = 1
			);
    
		right.grob <- placeGrob(
			frame = right.grob,
			grob = legend$right$fun,
			row = 1,
			col = 2
			);
		}
	else {
		right.grob <- y.label.right;
		}
  
	### BOTTOM GROB
	if (!is.null(legend$bottom$fun)) { 
		height.legend <- convertUnit(
			grobHeight(legend$bottom$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		height.text <- convertUnit(
			grobHeight(x.label),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		bottom.layout.final <- grid.layout(
			nrow = 2,
			ncol = 1,
			heights = unit(
				x = c(height.text, height.legend),
				units = c('lines','lines'),
				data = list(NULL, NULL)
				),
			widths = unit(1, 'null'),
			respect = FALSE
			);
    
		bottom.grob <- frameGrob(layout = bottom.layout.final)
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = x.label,
			row = 1,
			col = 1
			);
		bottom.grob <- placeGrob(
			frame = bottom.grob,
			grob = legend$bottom$fun,
			row = 2,
			col = 1
			);
		}
	else {
		bottom.grob <- x.label;
		}
  
	### TOP GROB
	if (!is.null(legend$top$fun)) { 
		height.legend <- convertUnit(
			grobHeight(legend$top$fun),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		height.text <- convertUnit(
			grobHeight(main.label),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		top.layout.final <- grid.layout(
			nrow = 3,
			ncol = 1,
			heights = unit(
				x = c(height.text,main.padding,height.legend),
				units = c('lines','lines','lines'),
        			data = list(NULL, NULL,NULL)
      				),
			widths = unit(1, 'null'),
			respect = FALSE
			);
    
		top.grob <- frameGrob(layout = top.layout.final)
		top.grob <- placeGrob(
			frame = top.grob,
			grob = rectGrob(gp=gpar(col="white", alpha = 0)),
			row = 2,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 1,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = legend$top$fun,
			row = 3,
			col = 1
			);
		}
	else {
    
		height.text <- convertUnit(
			grobHeight(main.label),
			unitTo = 'lines',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
    
		top.layout.final <- grid.layout(
			nrow = 2,
			ncol = 1,
			heights = unit(
				x = c(height.text,main.padding),
				units = c('lines','lines'),
				data = list(NULL, NULL)
				),
			widths = unit(1, 'null'),
			respect = FALSE
			);
		top.grob <- frameGrob(layout = top.layout.final)
		top.grob <- placeGrob(
			frame = top.grob,
      			grob = rectGrob(gp=gpar(col="white", alpha = 0)),
      			row = 1,
      			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 2,
			col = 1
			);
		}

	# create grob
	grob <- arrangeGrob(
		grobs=plot.objects,
		heights=heights, 
    		widths = widths, 
		ncol = layout.width,
		nrow=layout.height, 
		top = top.grob, 
		left = left.grob, 
		bottom = bottom.grob, 
		right = right.grob
		);
	# return grob
	if (!is.null(filename)) {
		BoutrosLab.plotting.general::write.plot(
			trellis.object = grob,
			filename = filename,
			height = height,
			width = width,
			size.units = size.units,
			resolution = resolution,
			enable.warnings = enable.warnings,
			description = description
			);
		}
	else {
		# return grob itself
		return (grob);
		}
	}

## function to get the grob width given the text and specification parameters##
get.text.grob.width <- function(labels, cex, rot, filename,width, height, resolution) {
  
	#grob size depends on image type and size -- must simulate opening the device
	extension <- file_ext(filename);
	if (!is.null(filename)) {
		if (extension == 'tiff') {
			tiff(height = height, width = width, res = resolution);
			}
		else if (extension=='png') {
			png(height = height, width = width, res = resolution);
			}
		else if (extension=='pdf') {
			cairo_pdf(height = height, width = width);
			}
		else if (extension=='svg') {
			svg(height = height, width = width);
			}
		else if (extension=='eps') {
			postscript(height = height, width = width);
			}
		else {
			stop('File type not supported');
			}
		}
	# if not an empty label or all blank, create the grob, and get its width
	if (length(labels) > 0 && !(all(labels == ''))) {
		grob <- textGrob(labels,gp=gpar(cex = cex,lineheight = 1), rot = rot, x = c(rep(0.5, length(labels))), y = c(rep(0.5, length(labels))));
		widthGrob <- convertUnit(
			grobWidth(grob),
			unitTo = 'points',
			axisFrom = 'x',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
		}
	else {
		widthGrob <- 0;
		}
	if (!is.null(filename)) {
		dev.off();
		}

	return (widthGrob);  

	}
## function to get the grob height given the text and specification parameters##
get.text.grob.height <- function(labels, cex, rot, filename, width, height, resolution) {
  
	#grob size depends on image type and size -- must simulate opening the device
	extension <- file_ext(filename);
	if (!is.null(filename)) {
		if (extension == 'tiff') {
			tiff(height = height, width = width, res = resolution);
			}
		else if (extension=='png') {
			png(height = height, width = width, res = resolution);
			}
		else if (extension=='pdf') {
			cairo_pdf(height = height, width = width);
			}
		else if (extension=='svg') {
			svg(height = height, width = width);
			}
		else if (extension=='eps') {
			postscript(height=height,width=width);
			}
		else {
			stop('File type not supported');
			}
		}
	# if not an empty label or all blank, create the grob, and get its height
	if (length(labels) > 0 && !(all(labels == ''))) {
		grob <- textGrob(labels,gp=gpar(cex = cex,lineheight = 1), rot = rot, x = c(rep(0.5, length(labels))), y = c(rep(0.5, length(labels))));
		heightGrob <- convertUnit(
			grobHeight(grob),
			unitTo = 'points',
			axisFrom = 'y',
			typeFrom = 'dimension',
			valueOnly = TRUE
			);
		}
	else {
		heightGrob <- 0;
		}

	if (!is.null(filename)) {
		dev.off();
		}
  
	return (heightGrob);
  
	}


