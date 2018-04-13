print.to.file <- function(dirname, funcname,data, filename) {
        user <- system("whoami",intern = T);
        out.filename <- 'data-df.tsv';
	
        numeric.data <- c();
        num.numeric <- 0;
        num.factor <- 0;
        num.integer <- 0;
	num.rows <- 0;
	num.cols <- 0;

        if(is.null(filename)) {
                filename <- 'None';
                }

	if(class(data) == 'list') {
		num.numeric <- length(data);
                numeric.data <- unlist(data);
                num.rows <- length(data[[1]]);
                num.cols <- length(data);
		}
	else if(class(data) == 'numeric') {
		num.numeric <- 1;
		numeric.data <- data;
		num.rows <- length(data);
		num.cols <- 1;
		}
        else if(class(data) == 'data.frame' || class(data) == 'matrix') {
		num.rows <- nrow(data);
		num.cols <- ncol(data);
                for(i in 1:num.cols) {
                        if(class(data[,i]) == 'numeric') {
                                num.numeric = num.numeric + 1;
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

	df.to.add <- NULL;
        if(num.numeric == 0) {
        	df.to.add <- data.frame(user = c(user), filename = c(filename), func.name = c(funcname),
                        data.type = c(class(data)), nrow = num.rows, ncol = num.cols, numeric = c(num.numeric),
                        factor = c(num.factor), integer = c(num.integer), max = c(0), min = c(0), median = c(0))
                }
        else {
                df.to.add <- data.frame(user = c(user), filename = c(filename), func.name = c(funcname),
                        data.type = c(class(data)), nrow = num.rows, ncol = num.cols, numeric = c(num.numeric),
                        factor = c(num.factor), integer = c(num.integer), max = c(max(numeric.data,na.rm = T)), min = c(min(numeric.data,na.rm=T)), median = c(median(numeric.data, na.rm = T)));
                }
        if(!is.null(df.to.add)) {
                if(!file.exists(paste(dirname,out.filename, sep="/"))) {
                        write.table(df.to.add,paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = T);
                        }
                else {
                        datainfo.dataframe <- read.table(paste(dirname,out.filename, sep="/"), header = T, fill = T);
                        if(!is.na(match(filename,datainfo.dataframe$filename))) {
                                datainfo.dataframe[match(filename,datainfo.dataframe$filename),] <- df.to.add;
                                write.table(datainfo.dataframe, paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = T);
                                }
                        else {
                                write.table(df.to.add, paste(dirname,out.filename, sep="/"), sep = '\t', row.names = F, col.names = F, append = T);
                                }
                        }
                }
        }
