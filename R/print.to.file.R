print.to.file <- function(dirname, funcname,data, filename) {
	user <- system("whoami",intern = T);
	out.filename <- 'data-df.tsv';
	if(is.null(filename)) {
		filename = 'None';
		}
	if(class(data) == 'data.frame' || class(data) == 'matrix') {

                data.types <- c();
                max.numeric <- NULL;
                min.numeric <- NULL;
		numeric.data <- c();
		num.numeric = 0;
		num.factor = 0;
		num.integer = 0;
		for(i in 1:ncol(data)) {
			data.types <- c(data.types,class(data[,i]));
			if(class(data[,i]) == 'numeric') {
				if(is.null(max.numeric)) {
					max.numeric <- max(data[,i], na.rm = T);
					}
				if(is.null(min.numeric)){ 
					min.numeric <- min(data[,i], na.rm = T); 
					}
			        num.numeric = num.numeric + 1;	
				max.numeric <- max(max.numeric, max(data[,i]), na.rm = T);
				min.numeric <- min(min.numeric, min(data[,i]), na.rm = T);
				numeric.data <- c(numeric.data, data[,i]);
				}
			else if (class(data[,i]) == 'integer') {
				num.integer = num.integer + 1;
				}
			else if(class(data[,i]) == 'factor') {
				num.factor = num.factor + 1;
				}
			}
			
		df.to.add <- NULL;
		if(is.null(max.numeric) || is.null(min.numeric)) {
                        df.to.add <- data.frame(user = c(user), filename = c(filename), func.name = c(funcname),
                        	data.type = c(class(data)), nrow = c(nrow(data)), ncol = c(ncol(data)), numeric = c(num.numeric),
                        	factor = c(num.factor), integer = c(num.integer), max = c(0), min = c(0), median = c(0))
			}
		else {
			df.to.add <- data.frame(user = c(user), filename = c(filename), func.name = c(funcname),
                                data.type = c(class(data)), nrow = c(nrow(data)), ncol = c(ncol(data)), numeric = c(num.numeric),
                                factor = c(num.factor), integer = c(num.integer), max = c(max(numeric.data,na.rm = T)), min = c(min(numeric.data,na.rm=T)), median = c(median(numeric.data, na.rm = T)));
			}
		if(!is.null(df.to.add)) {
			if(!file.exists(paste(dirname,out.filename, sep="/"))) {
				write.table(df.to.add,paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = T);
				}
			else {
				write.table(df.to.add,paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = F, append = T);
				}
			}
		}
	}

print.to.file.trellis.object <- function(dirname,trellis.object,filename) {
	user <- system("whoami",intern = T);
        out.filename <- 'data-df.tsv';
        if(is.null(filename)) {
                filename = 'None';
                }

	
	datainfo.dir.name <- paste('/.mounts/labs/boutroslab/private/BPGRecords/Objects', Sys.Date(), sep = '_');
        datainfo.dataframe <- read.table(paste(datainfo.dir.name, out.filename, sep="/"), header = T);
        parent.id <- nrow(datainfo.dataframe);	
	xaxis.labels.cex <- trellis.object$x.scales$cex[1];
	xaxis.labels.num.at <- length(trellis.object$x.scales$at);
	xaxis.labels.at.start <- trellis.object$x.scales$at[1];
	xaxis.labels.at.stop <- trellis.object$x.scales$at[length(trellis.object$x.scales$at)];
        
	if(xaxis.labels.num.at == 1 && is.logical(xaxis.labels.at.start) && is.logical(xaxis.labels.at.stop)) {
                labels = pretty(trellis.object$panel.args[[1]]$x);
                xaxis.labels.num.at <- length(labels);
                xaxis.labels.at.start <- labels[1];
                xaxis.labels.at.stop <- labels[length(labels)];
                }

	xaxis.labels.rot <- trellis.object$x.scales$rot[1];
	xaxis.labels.tck <- trellis.object$x.scales$tck[1];
	xaxis.labels.tick.number <- trellis.object$x.scales$tick.number;
	
        yaxis.labels.cex <- trellis.object$y.scales$cex[1];
        yaxis.labels.num.at <- length(trellis.object$y.scales$at);
        yaxis.labels.at.start <- trellis.object$y.scales$at[1];
        yaxis.labels.at.stop <- trellis.object$y.scales$at[length(trellis.object$y.scales$at)];
	
	if(yaxis.labels.num.at == 1 && is.logical(yaxis.labels.at.start) && is.logical(yaxis.labels.at.stop)) {
                labels = pretty(trellis.object$panel.args[[1]]$y);
                yaxis.labels.num.at <- length(labels);
                yaxis.labels.at.start <- labels[1];
                yaxis.labels.at.stop <- labels[length(labels)];
                }
        
	yaxis.labels.rot <- trellis.object$y.scales$rot[1];
        yaxis.labels.tck <- trellis.object$y.scales$tck[1];
        yaxis.labels.tick.number <- trellis.object$y.scales$tick.number;
	
	num.xlimits <- length(trellis.object$x.limits);
	xlimits.start <- trellis.object$x.limits[1];
	xlimits.end <- trellis.object$x.limits[length(trellis.object$x.limits)];
	
	if(num.xlimits > 2) {
                xlimits.start <- 0;
                xlimits.end <- num.xlimits + 1;
                }	
        
	num.ylimits <- length(trellis.object$y.limits);
        ylimits.start <- trellis.object$y.limits[1];
        ylimits.end <- trellis.object$y.limits[length(trellis.object$y.limits)];
	

	if(num.ylimits > 2) {
                ylimits.start <- 0;
                ylimits.end <- num.ylimits + 1;
                }

	df.to.add <- data.frame(
		user = c(user), 
		filename = c(filename),
		parent.id = c(parent.id), 
		xaxis.labels.cex = c(xaxis.labels.cex), 
		xaxis.labels.num.at = c(xaxis.labels.num.at),
		xaxis.labels.at.start = c(xaxis.labels.at.start), 
		xaxis.labels.at.stop = c(xaxis.labels.at.stop), 
		xaxis.labels.rot = c(xaxis.labels.rot),
		xaxis.labels.tck = c(xaxis.labels.tck), 
		xaxis.labels.tick.number = c(xaxis.labels.tick.number), 
		yaxis.labels.cex = c(yaxis.labels.cex), 
		yaxis.labels.num.at = c(yaxis.labels.num.at), 
		yaxis.labels.at.start = c(yaxis.labels.at.start), 
		yaxis.labels.at.stop = c(yaxis.labels.at.stop), 
		yaxis.labels.rot = c(yaxis.labels.rot), 
		yaxis.labels.tck = c(yaxis.labels.tck), 
		yaxis.labels.tick.number = c(yaxis.labels.tick.number),
		num.xlimits = c(num.xlimits), 
		xlimits.start = c(xlimits.start), 
		xlimits.end = c(xlimits.end), 
		num.ylimits = c(num.ylimits), 
		ylimits.start = c(ylimits.start),
		ylimits.end = c(ylimits.end)
		);
	
	if(!is.null(df.to.add)) {
        	if(!file.exists(paste(dirname,out.filename, sep="/"))) {
                	write.table(df.to.add,paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = T);
                        }
                else {
                        write.table(df.to.add,paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = F, append = T);
                        } 
                }

	}

