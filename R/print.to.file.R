print.to.file <- function(dirname, funcname,data, filename) {
	user <- system("whoami",intern = T);
	out.filename <- paste0('data-',user,sep = '.txt');
	if(class(data) == 'data.frame' || class(data) == 'matrix') {
		cat(capture.output(cat(paste0('user:', user, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
                cat(capture.output(cat(paste0('filename:', filename[1], sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		cat(capture.output(cat(paste0('func.name:', funcname, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		cat(capture.output(cat(paste0('data.type:', class(data), sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		cat(capture.output(cat(paste0('nrow:', nrow(data), sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
        	cat(capture.output(cat(paste0('ncol:', ncol(data), sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
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
		
		}	
		cat(capture.output(cat(paste0('numeric:', num.numeric, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		cat(capture.output(cat(paste0('factor:', num.factor, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		cat(capture.output(cat(paste0('integer:', num.integer, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
		if(is.null(max.numeric) || is.null(min.numeric)) {
			cat(capture.output(cat(paste0('max:', 0, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
                        cat(capture.output(cat(paste0('min:', 0, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
                        cat(capture.output(cat(paste0('median:', 0, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
			}
		else {
			cat(capture.output(cat(paste0('max:', max.numeric, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
        		cat(capture.output(cat(paste0('min:', min.numeric, sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
			cat(capture.output(cat(paste0('median:', median(numeric.data, na.rm = T), sep = '\n')), file=paste(dirname,out.filename, sep="/"), append = T));
			}
	}
