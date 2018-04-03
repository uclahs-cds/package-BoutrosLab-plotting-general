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
        datainfo.dataframe <- read.table(paste(datainfo.dir.name, out.filename, sep="/"), header = T, fill = T);
        parent.id <- nrow(datainfo.dataframe);	
	xaxis.labels.cex <- trellis.object$x.scales$cex[1];

        yaxis.labels.cex <- trellis.object$y.scales$cex[1];
	
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
		user = user, 
		filename = filename,
		parent.id = parent.id, 
		xaxis.labels.cex = xaxis.labels.cex, 
       		yaxis.labels.cex = yaxis.labels.cex, 
		xlimits.start = xlimits.start, 
		xlimits.end = xlimits.end, 
		ylimits.start = ylimits.start,
		ylimits.end = ylimits.end
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
