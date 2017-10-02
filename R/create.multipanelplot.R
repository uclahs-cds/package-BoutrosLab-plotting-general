### Custom printing function for multipanel object

print.multipanel <- function(x, ...){
		## set class to that of a grid object
		class(x) <- c('gtable', 'gTree', 'grob','gDesc')
		## close previous items if exist
		if(!is.null(dev.list())){
			dev.off()
			}
		## draw the plot like a grob
  		grid.draw(x)
		}

create.multipanelplot<-function(plot.objects = NULL, filename = NULL,height = 10, width = 10, resolution = 1000, 
                            heights = c(rep(1,layout.height)),widths = c(rep(1,layout.width)), layout.width = 1, 
                            layout.height = length(plot.objects), main = '', main.padding = 2,main.x = 0.5, main.y = 0.5, x.spacing = 0, y.spacing= 0,
                            xlab.label = '', xlab.cex = 2, ylab.label = '', ylab.label.right = '', 
                            ylab.cex = 2, main.cex = 3, legend = NULL, left.padding = 3,  
                            ylab.axis.padding = c(rep(0,layout.width)),xlab.axis.padding = c(rep(0,layout.height)), bottom.padding = 0, 
				top.padding = 0, right.padding = 0,layout.skip = c(rep(FALSE, layout.width*layout.height)),
                            description = 'Created with BoutrosLab.plotting.general', size.units = 'in',enable.warnings = FALSE) {
	## make axis.padding appropriate length if only 1 value specified
	if(length(ylab.axis.padding) == 1) {
		ylab.axis.padding <- rep(ylab.axis.padding,layout.width);

		}
	if(length(xlab.axis.padding) == 1) {
		xlab.axis.padding <- rep(xlab.axis.padding,layout.height);
		}
	### ERROR CHECKING ###
	if (length(heights) != layout.height) {
		stop("heights must have layout.height  number of entries");
		}
	if (length(widths) != layout.width) {
		stop("widths must have layout.width  number of entries");
		}
	if (length(layout.skip) != layout.width*layout.height) {
		stop("layout.skip must have same number of entries as layout.width * layout.height");
		}
	if(!is.list(plot.objects)) {
		stop("plot.objects must be a list");
		}
	if(length(ylab.axis.padding) != layout.width) {
		stop("ylab.axis.padding must be the same size as layout.width");
		}
	if(length(xlab.axis.padding) != layout.height) {
		stop("xlab.axis.padding must be the same size as layout.height");
		}
	
	padding.text.to.padding.ratio <- 6; # this is used to align plots with diffrent label sizes
  	tick.to.padding.ratio <- 0.9484252; # this is used to evaluate length of ticks (is equivalent to 1mm)
	additional.padding <- 0.5 # this is additional padding for labels that goes above calculation (takes care of overhanging values)
	### MINOR ADJUSTMENTS TO MAKE VALUES SEEM MORE LOGICAL
	x.spacing <- x.spacing/10;
	y.spacing <- y.spacing/10;

	### HANDLE SKIP LAYOUT ###
	newplots <- list();
	plotnum <- 1;
	
	## ADD BLANKS FOR LAYOUT.SKIP
	for(i in c(1:(layout.width*layout.height))) {
		if (plotnum <= length(plot.objects) && layout.skip[i] == FALSE) {
			newplots[i] <- plot.objects[plotnum];
			plotnum <- plotnum + 1;
			}
		else {
			newplots[[i]] <- rectGrob(gp=gpar(col="white", alpha = 0));
			}
		}
	plot.objects <- newplots
	layout.height <- layout.height*2;
	layout.width <- layout.width*2;

	
	newplots <- list();
	plotnum <- 1;

	### AND IN BETWEEN GROBS TO REPRESENT X AND Y SPACING (PREVENTS PUSHING OF PLOTS)
	for(k in c(1:layout.height)) {
		for(i in c(1:layout.width)) {
			if(i %% 2 == 0 || k %% 2 == 0) {
				newplots[[i + (k-1)*(layout.width)]] <- rectGrob(gp=gpar(col="white", alpha = 0));
				}
			else {
				newplots[i + (k-1)*(layout.width)] <- plot.objects[plotnum];
				plotnum <- plotnum + 1;
				}

			}
		}
	plot.objects <- newplots
	
  
	### PAR SETTINGS EQUIVALENT ###
	for (i in c(1:length(plot.objects))) {
      		# make all paddings the same (in order to line them up) -- needed or plots wont line up properly
		if (!is.null(plot.objects[[i]]$par.settings)) {

			plot.objects[[i]]$par.settings$layout.widths$left.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$key.left <- 0;
			plot.objects[[i]]$par.settings$layout.widths$key.ylab.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$ylab <- 0;
			plot.objects[[i]]$par.settings$layout.widths$ylab.axis.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.left <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.right <- 0;
			plot.objects[[i]]$par.settings$layout.widths$axis.key.padding <- 0;
			plot.objects[[i]]$par.settings$layout.widths$key.right <- 0; #
			plot.objects[[i]]$par.settings$layout.widths$right.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$top.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$main <- 0;
 			plot.objects[[i]]$par.settings$layout.heights$main.key.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$key.top <- 0; #
			plot.objects[[i]]$par.settings$layout.heights$key.axis.padding <-  0; #
			plot.objects[[i]]$par.settings$layout.heights$axis.top <- 0;
			plot.objects[[i]]$par.settings$layout.heights$axis.bottom <- 0;
			plot.objects[[i]]$par.settings$layout.heights$axis.xlab.padding <- 0;
			plot.objects[[i]]$par.settings$layout.heights$xlab <- 0;
			plot.objects[[i]]$par.settings$layout.heights$xlab.key.padding <- 0; #
			plot.objects[[i]]$par.settings$layout.heights$key.bottom <- 0; #
			plot.objects[[i]]$par.settings$layout.heights$key.sub.padding <- 0; #
			plot.objects[[i]]$par.settings$layout.heights$sub <- 0;
			plot.objects[[i]]$par.settings$layout.heights$bottom.padding <- 0;
			}
      		}
		
  

	### SPACING #####
	#make y spacing into an array of proper length
	if (length(y.spacing) == 1) {
		y.spacing <- rep(y.spacing,layout.height/2);
		}
	if (length(x.spacing) == 1) {
		x.spacing <- rep(x.spacing,layout.width/2);
		}

	# adjust widths to accuratly using the x and y spacing values
  	widths <- as.vector(rbind(widths,x.spacing))
	heights <- as.vector(rbind(heights,y.spacing))
	heights[length(heights)] <- 0
	widths[length(widths)] <- 0
	
	
  
	### PADDING ####
  
	#variables to represent amount of padding needed
	### X AND Y PLOT AXIS LABELS
	largest.y.axis <- list(); 
	largest.x.axis <- list();
	### TICKS
	left.ticks <- list(); 
	bottom.ticks <- list();
	### X AND Y AXIS LABELS
	y.label.size <- list(); 
	x.label.size <- list();
	### MAIN PLOT LABEL
	main.size <- list();
	for (i in  c(1:length(plot.objects))) {
    
		largest.y.axis[i] <- 0;
		left.ticks[i] <- 0;
		largest.x.axis[i] <- 0;
		bottom.ticks[i] <- 0;
		x.label.size[i] <- 0;
		y.label.size[i] <- 0;
		main.size[i] <- 0
    		# set variables for each plot according to their respective plot values
		if (!is.null(plot.objects[[i]]$par.settings)) {
			### GET GROB INFO FOR SIMPLE VARS
			main.size[i] <- get.text.grob.height(plot.objects[[i]]$main$label,plot.objects[[i]]$main$cex,0,filename,width,height,resolution);
			y.label.size[i] <- get.text.grob.width(plot.objects[[i]]$ylab$label,plot.objects[[i]]$ylab$cex,90,filename, width,height,resolution);
			x.label.size[i] <- get.text.grob.height(plot.objects[[i]]$xlab$label,plot.objects[[i]]$xlab$cex,0,filename,width,height,resolution);
			left.ticks[i] <- plot.objects[[i]]$y.scales$tck[1]*tick.to.padding.ratio;
			bottom.ticks[i] <- plot.objects[[i]]$x.scales$tck[1]*tick.to.padding.ratio;
		 	### Labels can get a bit weird because they come in many shapes and forms (i.e, expressions, NA, NULL....) --- this is intended to check for all those
			if(!is.expression(plot.objects[[i]]$y.scales$labels[1])) {
				if (is.null(plot.objects[[i]]$y.scales$labels) || is.na(plot.objects[[i]]$y.scales$labels) || plot.objects[[i]]$y.scales$labels == TRUE ) {
					if (!is.null(plot.objects[[i]]$panel.args[[1]]$y)) {
						if (!is.null(levels(plot.objects[[i]]$panel.args[[1]]$y))) {
                                        		y.axis.labs <- levels(plot.objects[[i]]$panel.args[[1]]$y)
                                        		}
						else{
							yvals <- as.numeric(plot.objects[[i]]$panel.args[[1]]$y)
							y.axis.labs <-  pretty(yvals[is.finite(yvals)])
							}
						}
					else {
						y.axis.labs <- ''
						}
					}
				else {
					y.axis.labs <- plot.objects[[i]]$y.scales$labels
					}
				}
			else {
				y.axis.labs <- plot.objects[[i]]$y.scales$labels
				}

			largest.y.axis[i] <- get.text.grob.width(y.axis.labs,
								plot.objects[[i]]$y.scales$cex, 
								plot.objects[[i]]$y.scales$rot[1], 
								filename, width,height,resolution);
			
			if(!is.expression(plot.objects[[i]]$x.scales$labels[1])) {
				if (is.null(plot.objects[[i]]$x.scales$labels) || is.na(plot.objects[[i]]$x.scales$labels) || plot.objects[[i]]$x.scales$labels == TRUE) {
					if (!is.null(plot.objects[[i]]$panel.args[[1]]$x)) {
                                		if (!is.null(levels(plot.objects[[i]]$panel.args[[1]]$x))){
                                        		x.axis.labs <- levels(plot.objects[[i]]$panel.args[[1]]$x)
                                        		}
						else{
							xvals <- as.numeric(plot.objects[[i]]$panel.args[[1]]$x)
							x.axis.labs <-  pretty(xvals[is.finite(xvals)])
							}
						}
					else{
						x.axis.labs <- ''
						}
					}
				else {
					x.axis.labs <- plot.objects[[i]]$x.scales$labels
					}
			}
			else {
                                x.axis.labs <- plot.objects[[i]]$x.scales$labels
                                }


			largest.x.axis[i] <- get.text.grob.height(x.axis.labs,
								 plot.objects[[i]]$x.scales$cex, 
								 plot.objects[[i]]$x.scales$rot[1], 
								 filename, width,height,resolution);
      			
			}
		}
  
	
	# add the correct amount to each plot based on the column (use ylab axis)
	for ( i in c(1:layout.width)) {
		maxAxis <- max(unlist(largest.y.axis[seq(i,length(plot.objects),layout.width)]));
		maxTicks <- max(unlist(left.ticks[seq(i,length(plot.objects),layout.width)]));
		maxLabels <- max(unlist(y.label.size[seq(i,length(plot.objects),layout.width)]));
		for ( j in seq(i,length(plot.objects),layout.width)) {
			to.add <- (maxAxis + maxLabels) / padding.text.to.padding.ratio + maxTicks + additional.padding;
			if (!is.null(plot.objects[[j]]$par.settings)) {
				plot.objects[[j]]$par.settings$layout.widths$ylab.axis.padding <- ylab.axis.padding[ceiling(i/2)] + to.add;
				}
			}
		}
  
  
	# add the correct amount to each plot based on the row (use xlab axis)
	for ( i in seq(1,length(plot.objects),layout.width)) {
		maxAxis <- max(unlist(largest.x.axis[seq(i,i + layout.width - 1,1)]));
		maxTicks <- max(unlist(bottom.ticks[seq(i,i + layout.width - 1,1)]));
		maxLabels <- max(unlist(x.label.size[seq(i,i + layout.width - 1,1)]));
		maxMain <- max(unlist(main.size[seq(i,i + layout.width - 1,1)]))/2;
		for ( j in c(i:(i + layout.width - 1))) {
			to.add <- (maxAxis + maxLabels) / padding.text.to.padding.ratio + maxTicks + additional.padding;
			if (j <= length(plot.objects) && !is.null(plot.objects[[j]]$par.settings)) {
				plot.objects[[j]]$par.settings$layout.heights$axis.xlab.padding <- xlab.axis.padding[ceiling(i/(layout.width*2))] + to.add;
				if(maxMain != 0 && (is.null(plot.objects[[j]]$main$label) || plot.objects[[j]]$main$label == '')) {
					plot.objects[[j]]$main$label <- '\t'; #make sure it thinks a label is there 
					}
				plot.objects[[j]]$par.settings$layout.heights$main <- plot.objects[[j]]$par.settings$layout.heights$main+(maxMain/padding.text.to.padding.ratio);
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
			nrow = 4,
			ncol = 1,
			heights = unit(
				x = c(top.padding,height.text,main.padding,height.legend),
				units = c('lines','lines','lines','lines'),
        			data = list(NULL, NULL,NULL,NULL)
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
			grob = rectGrob(gp=gpar(col="white", alpha = 0)),
			row = 3,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 2,
			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = legend$top$fun,
			row = 4,
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
			nrow = 3,
			ncol = 1,
			heights = unit(
				x = c(top.padding,height.text,main.padding),
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
      			grob = rectGrob(gp=gpar(col="white", alpha = 0)),
      			row = 1,
      			col = 1
			);
		top.grob <- placeGrob(
			frame = top.grob,
			grob = main.label,
			row = 3,
			col = 1
			);
		}

	# create grob of all plots
	grob <- arrangeGrob(
		grobs = plot.objects,
		heights=heights, 
    		widths = widths,
		ncol = layout.width,
		nrow=layout.height, 
		top = top.grob, 
		left = left.grob, 
		bottom = bottom.grob, 
		right = right.grob
		);
	## Add white background color
	grob <- gtable_add_grob(grob,grobs = rectGrob(gp=gpar(fill="white", lwd=0)),1,1,nrow(grob),ncol(grob),0)

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
		class(grob) <- "multipanel"
		# return grob itself
		return (grob);
		}
	}



### function to get the grob width given the text and specification parameters##
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
	if (length(labels) > 0 && !(all(toString(labels) == ''))) {
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
	### make sure to turn off the dev or we will have one open for every time this is called
	if (!is.null(filename)) {
		dev.off();
		}

	return (widthGrob);  

	}

### function to get the grob height given the text and specification parameters##
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
	if (length(labels) > 0 && !(all(toString(labels) == ''))) {
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
	### make sure to turn off the dev or we will have one open for every time this is called
	if (!is.null(filename)) {
		dev.off();
		}
  
	return (heightGrob);
  
	}

